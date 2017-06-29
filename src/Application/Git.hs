----------------------------------------------------------------------------
-- |
-- Module      :  Application.Git
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  25 January 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Application.Git (runGit) where

import Codec.Compression.Zlib as Zlib
import Control.Applicative
import Control.Arrow as Arr
import Control.Exception as Exc
import Control.Monad
import Control.Monad.Except
import Data.Algorithm.Diff as Diff
import Data.Binary.Get as BinGet
import Data.Binary.Put as BinPut
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import Data.Char
import qualified Data.Digest.Pure.SHA as SHA
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.LocalTime
import Data.Word
import Numeric (readHex)
import Options.Applicative
import System.Console.ANSI as TTY
import System.Directory as Dir hiding (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.FilePath.Glob as Glob
import System.IO
import System.Posix as Posix
import System.Process as Proc
import System.Time
import Text.Printf
import Text.Read

import Application.Git.Errors
import Application.Git.RepoDirectory
import Data.Char.Ext
import Data.Git.BlobType
import Data.Git.CommitObject
import Data.Git.SHAHash

int :: (Num b, Integral a) => a -> b
int = fromIntegral

bool :: a -> a -> Bool -> a
bool thenb elseb cond = if cond then thenb else elseb

maybeOr :: String -> Maybe a -> a
maybeOr msg = fromMaybe (error msg)

colorPutStrLn :: Color -> String -> IO ()
colorPutStrLn color msg = do
  setSGR [SetColor Foreground Dull color]
  putStr msg
  setSGR []
  putStrLn ""

todFromPosix :: Show a => a -> ClockTime
todFromPosix etime = TOD (read ssec) psec
  where
    (ssec, _:s') = L.break (not . isDigit) (show etime)
    psec           = case readEither s' of
      Left _     -> 0
      Right nsec -> 1000 * nsec

timespecToTOD :: (Word32, Word32) -> ClockTime
timespecToTOD (tv_sec, tv_nsec) = TOD (toInteger tv_sec) (1000 * toInteger tv_nsec)

timespecFromTOD :: ClockTime -> (Word32, Word32)
timespecFromTOD (TOD sec psec) = (fromInteger sec, fromInteger (psec `div` 1000))

type PackInfoMap = Map SHAHash' (Int, Int, Word32) -- (packOffset, packSize, crc32)

type PackInfo = (FilePath, PackInfoMap)

data IndexEntry = IndexEntry
  { indCTime    :: ClockTime
  , indMTime    :: ClockTime
  , indDev      :: Word32
  , indIno      :: Word32
  , indMode     :: Word32
  , indUID      :: Word32
  , indGID      :: Word32
  , indFileSize :: Word32
  , indSHA      :: SHAHash
  , indFl       :: Word16
  , indFName    :: FilePath
  } deriving (Eq, Ord, Show)

data BlobMode =
    FileBlob
  | ExecBlob
  | SymlinkBlob
  | GitlinkBlob
  | UnknownBlob String
  deriving (Eq, Ord, Show)

blobModePermissions :: BlobMode -> String
blobModePermissions = \case
  FileBlob        -> "100644"
  ExecBlob        -> "100755"
  SymlinkBlob     -> "120000"
  GitlinkBlob     -> "160000"
  UnknownBlob mod -> mod

indmodeToBlobMode :: Word32 -> Maybe BlobMode
indmodeToBlobMode = (`M.lookup` modes)
  where
    modes :: Map Word32 BlobMode
    modes = M.fromList
      [ (0o100644, FileBlob)
      , (0o100755, ExecBlob)
      , (0o120000, SymlinkBlob)
      , (0o160000, GitlinkBlob)
      ]

data GitTree =
    GitBlob BlobMode SHAHash FilePath
  | GitTree SHAHash FilePath [GitTree]
  deriving Show

data FSTree =
    FSDir FilePath [FSTree]
  | FSFile FilePath
  deriving Show

objectPath :: RepoDirectory -> SHAHash' -> FilePath
objectPath gitDir sha =
  unRepoDirectory gitDir ++ "/" ++ gitObjectPath sha

doesObjExist :: RepoDirectory -> [PackInfo] -> SHAHash' -> IO Bool
doesObjExist gitdir idxmaps sha
  | any (M.member sha . snd) idxmaps = pure True
  | otherwise                        = doesFileExist (objectPath gitdir sha)


readFile'
  :: (FileNotFoundError e, MonadError e m, MonadIO m)
  => FilePath
  -> m B.ByteString
readFile' path = do
  exists <- liftIO $ doesFileExist path
  if exists
  then liftIO $ B.readFile path
  else throwError $ mkFileNotFoundError path

getHeadRef
  :: (FileNotFoundError e, InvalidHEADError e, MonadError e m, MonadIO m)
  => RepoDirectory
  -> m FilePath
getHeadRef gitdir = do
  contents <- readFile' gitHead
  case C8.stripPrefix "ref: " contents of
    Nothing   -> throwError $ mkInvalidHEADError contents
    Just path -> pure $ C8.unpack $ C8.takeWhile (not . isNewline) path
  where
    gitHead = unRepoDirectory gitdir </> "HEAD"

getHeadSHA
  :: (FileNotFoundError e, InvalidHEADError e, InvalidSHAError e, MonadError e m, MonadIO m)
  => RepoDirectory
  -> m SHAHash'
getHeadSHA gitdir = do
  reffile <- (unRepoDirectory gitdir </>) <$> getHeadRef gitdir
  shaFromByteString =<< readFile' reffile

getHeadTree
  :: (FileNotFoundError e, InvalidHEADError e, InvalidSHAError e, MonadError e m, MonadIO m)
  => RepoDirectory
  -> [PackInfo]
  -> m SHAHash'
getHeadTree gitdir idxmaps = do
  sha                          <- getHeadSHA gitdir
  Blob{blobType, blobContents} <- loadBlob gitdir idxmaps sha
  case blobType of
    BlobCommit -> commitObjectTree <$> parseCommitObjectFromLazy blobContents
    _          -> throwError $ mkInvalidObjectTypeError BlobCommit blobType sha

blobify :: String -> BL.ByteString -> BL.ByteString
blobify blobty objdata = BL.append (BLU.fromString (blobty ++ " " ++ show (BL.length objdata) ++ "\0")) objdata

writeObject objpath obj = do
    createDirectoryIfMissing False $ takeDirectory objpath
    BL.writeFile objpath (Zlib.compress obj)
    setFileMode objpath 0o100444

data Blob = Blob
  { blobType     :: BlobType
  , blobLen      :: !Int
  , blobContents :: BL.ByteString
  } deriving (Eq, Ord, Show)

loadBlob
  :: MonadIO m
  => RepoDirectory
  -> [PackInfo]
  -> SHAHash'
  -> m Blob
loadBlob gitdir idxmaps hash = do
  let path :: FilePath
      path = objectPath gitdir hash
  isobj <- liftIO $ doesFileExist path
  if isobj
  then parseBlob . Zlib.decompress <$> BL.readFile path
  else do
    let (idxfile, idxmap)   = head $ filter ((hash `M.member`) . snd) idxmaps
        packfile :: FilePath
        packfile            = unRepoDirectory gitdir ++ "/objects/pack/" ++ replaceExtension idxfile "pack"
        skipblobinfo (t, n) = getWord8 >>= ((bool (skipblobinfo (t, n+1)) (pure (t, n))) . (`testBit` 7))
        blobinfo            = getWord8 >>= (\w -> (if w `testBit` 7 then skipblobinfo else pure) (w, 1))
        getblob blobpos blobsz = do
           skip blobpos
           (ty, skipped) <- blobinfo
           zblob         <- getByteString (blobsz - skipped)
           pure (ty, BL.fromStrict zblob)

        Just (blobpos, blobsz, _) = M.lookup hash idxmap
    (ty, zblob) <- runGet (getblob blobpos blobsz) <$> BL.readFile packfile
    let blob        = Zlib.decompress zblob
        Just blobty = L.lookup (ty .&. 0x70) [(0x10,"commit"), (0x20,"tree"), (0x30,"blob"), (0x40,"tag"), (0x60,"ofsdel"), (0x70, "refdel")]
    pure (blobty, int $ BL.length blob, blob)

-- writeBlob :: FilePath -> [(FilePath, PackInfoMap)] -> String -> BL.ByteString -> IO SHAHash'
-- writeBlob gitdir idxmaps blobty blob = do
--   let obj = blobify blobty blob
--       sha :: SHAHash'
--       sha = hashobj' obj
--   exists <- doesObjExist gitdir idxmaps sha
--   unless exists $ do
--     let path :: FilePath
--         path = objectPath gitdir sha
--     putStrLn $ "### writing : " ++ path
--     writeObject path obj
--   pure sha
--
parseBlob :: BL.ByteString -> (String, Int, BL.ByteString) -- blobtype, bloblen, blob
parseBlob str = (BLU.toString btype, read $ BLU.toString slen, BL.tail tl)
  where
    (btype, tl') = BL.break (== 0x20) str ; (slen, tl) = BL.break (== 0) tl'
--
-- parseTreeObject :: BL.ByteString -> [(String, FilePath, SHAHash)]
-- parseTreeObject = L.unfoldr parseEntry . BL.unpack -- [(mode::String, name::FilePath, hash::SHAHash)]
--   where
--     parseEntry = \case
--       [] -> Nothing
--       bl -> Just ((BU.toString $ B.pack mode, BU.toString $ B.pack path, B.pack hsh), tl')
--         where
--           (hd, _:tl) = splitAt (fromJust $ L.findIndex (== 0) bl) bl
--           (mode, _:path) = break (== 0x20) hd ; (hsh, tl') = splitAt 20 tl
--
--
-- dumpTreeObject :: [(String, FilePath, SHAHash)] -> BL.ByteString
-- dumpTreeObject = runPut . void . mapM dumpTreeEntry . L.sortBy comparator
--   where
--     comparator = compare `on` (\(m,e,_) -> BU.fromString $ e ++ bool "/" "" (m == "40000"))
--     dumpTreeEntry (mod, name, sha) = putByteString (BU.fromString $ mod ++ " " ++ name) >> putWord8 0 >> putByteString sha
--
-- prettyTreeObject :: [(String, FilePath, SHAHash)] -> String
-- prettyTreeObject = unlines . map (\(mode, path, hash) -> concat [ty mode, " ", showSHA hash, "    ", path])
--   where ty mod = maybeOr ("wrong tree entry type : " ++ mod) $ L.lookup mod blobtypes
--         blobtypes = [("100644","100644 blob"), ("40000","040000 tree")]
--
-- data CommitObject = ComitObject
--   { commitObjectTree         :: SHAHash'
--   , commitObjectParent       :: NonEmpty SHAHash'
--   , commitObjectAuthor       :: B.ByteString
--   , commitObjectCommitter    :: B.ByteString
--   , commitObjectGPGSignature :: Maybe B.ByteString
--   , commitObjectMessage      :: [String]
--     -- [("tree", showSHA treesha),("parent", prevcommit),("author", cmAuthor),("committer", cmAuthor)]
--   }
--
-- parseCommitObject :: BL.ByteString -> (Map String String, [String])
-- parseCommitObject blob =
--   (M.fromList $ map (\ln -> let (hdr:tl) = words ln in (hdr, unwords tl)) commMeta, commMsg)
--   where
--     (commMeta, commMsg) = break null $ lines $ BLU.toString blob
--
-- getIdxFile_v2 :: Get (Map SHAHash (Int, Word32))
-- getIdxFile_v2 = do
--     indv <- replicateM 0x100 getWord32be
--     let lastind = int $ last indv
--     hashv  <- replicateM lastind (getByteString 20)
--     crc32v <- replicateM lastind getWord32be
--     offv   <- map int <$> replicateM lastind getWord32be
--     -- TODO: 8b offsets
--     pure $ M.fromAscList $ zip hashv $ zip offv crc32v
--
-- parseIdxFile_v2 :: FilePath -> IO PackInfoMap -- (offset, size, crc32)
-- parseIdxFile_v2 idxfile = do
--     idxdata <- BL.readFile idxfile
--     packlen <- int . fileSize <$> getFileStatus (replaceExtension idxfile "pack")
--     let (idxbody, trail) = BL.splitAt (BL.length idxdata - 20) idxdata
--     when (show (SHA.sha1 idxbody) /= showSHA (BL.toStrict trail)) $
--       error "idxfile: idx hash invalid"
--     let (0xff744f63, 2, idxmap') = runGet (liftA3 (,,) getWord32be getWord32be getIdxFile_v2) idxbody
--     let offs' = S.fromList $ map fst (M.elems idxmap') ++ [packlen - 20]
--     return $ M.map (\(off, crc32) -> (off, fromJust (S.lookupGT off offs') - off, crc32)) idxmap'
--
-- parseIndex :: BL.ByteString -> [IndexEntry]
-- parseIndex dat = map makeIdxentry idxdata
--   where
--     ("DIRC", _ver, nentries) = runGet (liftA3 (,,) (BU.toString <$> getByteString 4) getWord32be getWord32be) dat
--     go nb bs                 = (B.break (== 0) <$> getByteString nb) >>=
--                                (\(d, z) -> (if B.null z then go 8 else pure)(B.append bs d))
--     getIdxEntry              = liftM4 (,,,) (replicateM 10 getWord32be) (getByteString 20) getWord16be (go 2 B.empty)
--     idxdata                  = runGet (replicateM (int nentries) getIdxEntry) (BL.drop 12 dat)
--     makeIdxentry ([ctsec, ctusec, mtsec, mtusec, stdev, stino, stmode, stuid, stgid, fsize], sha, flags, fname) =
--       IndexEntry
--         { indCTime    = timespecToTOD (ctsec, ctusec)
--         , indMTime    = timespecToTOD (mtsec, mtusec)
--         , indDev      = stdev
--         , indIno      = stino
--         , indMode     = stmode
--         , indUID      = stuid
--         , indGID      = stgid
--         , indFileSize = fsize
--         , indSHA      = sha
--         , indFl       = flags
--         , indFName    = BU.toString fname
--         }
--     -- read extensions -- verify SHA
--
-- dumpIndex :: Map FilePath IndexEntry -> BL.ByteString
-- dumpIndex indmap = BL.append body trailer
--   where
--     body = runPut $ do
--       putByteString (BU.fromString "DIRC") >> mapM putWord32be [2, int $ M.size indmap]
--       mapM (putEntry . snd) . M.toAscList . M.mapKeys BU.fromString $ indmap
--       pure ()
--     trailer :: BLU.ByteString
--     trailer = SHA.bytestringDigest $ SHA.sha1 body
--     putEntry (IndexEntry ctime mtime dev ino mod uid gid fsize sha fl fname) = do
--       let ((cts, ctns), (mts, mtns)) = (timespecFromTOD ctime, timespecFromTOD mtime)
--           bname = BU.fromString fname
--           zpadding = 8 - ((62 + B.length bname) `rem` 8)
--       mapM_ putWord32be [int cts, int ctns, int mts, int mtns, dev, ino, mod, uid, gid, fsize]
--       putByteString sha >> putWord16be fl >> putByteString bname >> replicateM zpadding (putWord8 0)
--
-- hashFromGitTree :: [FilePath] -> GitTree -> Maybe SHAHash
-- hashFromGitTree [name] (GitTree _ _ entries) =
--   listToMaybe $ mapMaybe match entries
--   where
--     match entry = case entry of
--       GitBlob _ sha n
--         | n == name -> Just sha
--       _ -> Nothing
-- hashFromGitTree (dir:dirs) (GitTree _ _ entries) =
--   hashFromGitTree dirs =<< listToMaybe (mapMaybe match entries)
--   where
--     match entry = case entry of
--       GitTree _ d _ | d == dir -> Just entry
--       _ -> Nothing
--
-- loadTree :: FilePath -> [PackInfo] -> SHAHash -> FilePath -> IO GitTree
-- loadTree gitdir pathidx hash dirname = do
--   ("tree", _, blob) <- loadBlob gitdir pathidx hash
--   let loadSubtree (mod, name, sha) = if mod == "40000" || mod == "040000"
--         then loadTree gitdir pathidx sha name
--         else pure $ GitBlob (fromMaybe (UnknownBlob mod) $ lookup mod
--               [("100644",FileBlob), ("100755",ExecBlob), ("120000",SymlinkBlob), ("160000",GitlinkBlob)]) sha name
--   GitTree hash dirname <$> forM (parseTreeObject blob) loadSubtree
--
-- -- readTree :: GitTree -> IO [IndexEntry]
--
-- writeTree :: FilePath -> [PackInfo] -> GitTree -> IO SHAHash
-- writeTree _ _ (GitBlob _ sha _) = pure sha   -- a blob must have been written by `omit add` already
-- writeTree gitdir idxmaps (GitTree sha name entries) = do
--   let mkinfo e = case e of { GitBlob mod sha name -> (show mod, name, sha); GitTree sha name _ -> ("40000", name, sha) }
--   let treeblob = dumpTreeObject $ map mkinfo entries
--   let obj = blobify "tree" treeblob
--   let sha :: SHAHash'
--       sha = hashobj' obj
--   exists <- doesObjExist gitdir idxmaps sha
--   unless exists $ do
--     mapM_ (writeTree gitdir idxmaps) entries
--     writeObject (objectPath gitdir sha) obj
--   pure sha
--
-- commitTree :: FilePath -> [(String, String)] -> String -> IO SHAHash'
-- commitTree gitdir meta msg = do
--   let obj = blobify "commit"
--           $ BLU.fromString
--           $ unlines
--           $ map (\(hdr, inf) -> unwords [hdr, inf]) meta ++ [""] ++ lines msg
--   let sha :: SHAHash'
--       sha = hashobj obj
--   writeObject (objectPath gitdir sha) obj
--   pure sha
--
-- fsTreeFromList :: FilePath -> [[FilePath]] -> FSTree
-- fsTreeFromList dir fileparts = FSDir dir dirlst
--   where grps = map (\grp -> (head (head grp), map tail grp)) $ L.groupBy ((==) `on` head) fileparts
--         sublst fname = fsTreeFromList (dropTrailingPathSeparator fname)
--         dirlst = map (\(fname, subdirs) -> bool (FSFile fname) (sublst fname subdirs) $ null (head subdirs) ) grps
--
-- fsTreeFromDir :: FilePath -> FilePath -> [Glob.Pattern] -> IO FSTree
-- fsTreeFromDir path dir ignored = FSDir dir <$> catMaybes <$> (mapM fstreefy =<< getDirectoryContents (path </> dir))
--   where fstreefy name = if name `L.elem` [".", "..", ".git"] || L.any (flip Glob.match name) ignored
--           then pure Nothing else do
--           st <- getFileStatus (path </> dir </> name)
--           case st of
--             _ | isRegularFile st || Posix.isSymbolicLink st -> pure $ Just $ FSFile name
--             _ | isDirectory st -> Just <$> fsTreeFromDir (path </> dir) name ignored
--             _ -> pure Nothing
--
-- fsTreeFlatten :: FilePath -> FSTree -> [FilePath]
-- fsTreeFlatten cwd (FSFile fname) = [cwd </> fname]
-- fsTreeFlatten cwd (FSDir dname entries) = concat $ map (fsTreeFlatten (cwd </> dname)) entries
--
-- makeTreeFromIndex :: FilePath -> Map FilePath IndexEntry -> IO GitTree
-- makeTreeFromIndex root indexByPath = go root $ fsTreeFromList root $ map (splitPath . indFName . snd) $ M.toAscList indexByPath
--   where
--     go workdir (FSDir dir entries) = do
--       leaves <- forM entries $ \entry -> do
--         case entry of
--           FSFile fname -> do
--             let ie = indexByPath M.! path
--                 path' = makeRelative root workdir </> fname
--                 path = if "./" `L.isPrefixOf` path' then drop 2 path' else path'
--             case indmodeToBlobMode (indMode ie)  of
--               Nothing  -> error $ concat ["unknown mode ", show (indMode ie), " in index ", showSHA (indSHA ie)]
--               Just mod -> pure $ GitBlob mod (indSHA ie) fname
--           FSDir subdir _ -> go (workdir </> subdir) entry
--       let treeentrify (GitBlob mod sha fname) = (show mod, fname, sha)
--           treeentrify (GitTree sha dir _) = ("40000",  dir, sha)
--       -- mapM (\(mod, name, sha) -> putStrLn $ mod++" "++showSHA sha++": " ++name) $ map treeentrify leaves
--       let sha = hashobj $ blobify "tree" $ dumpTreeObject $ map treeentrify leaves
--       pure $ GitTree sha dir leaves
--
-- groupByAscRange :: [(Int, a)] -> [[a]]
-- groupByAscRange = reverse . map reverse . snd . L.foldl' go (0, [[]])
--   where go (n, grps@(hd:tl)) (k, v) = (k, if k == succ n then ((v : hd) : tl) else [v]:grps)
--
-- notFirst diffval = case diffval of { First _ -> False; _ -> True }
-- notSecond diffval = case diffval of { Second _ -> False; _ -> True }
-- isBoth diffval = case diffval of { Both _ _ -> True; _ -> False }
--
-- contextDiff :: Eq t => Int -> [Diff t] -> [[Diff (Int, t)]]
-- contextDiff nctx diff = groupByAscRange $ IM.toAscList ctxmap
--   where
--     annot (num1, num2, res) = \case
--       (Both ln1 ln2) -> (succ num1, succ num2, Both (num1,ln1) (num2,ln2) : res)
--       (First ln)     -> (succ num1, num2,      First (num1, ln) : res)
--       (Second ln)    -> (num1,      succ num2, Second (num2, ln) : res)
--     lnmap = IM.fromList $ zip [1..] $ reverse $ (\(_,_,e) -> e) $ L.foldl' annot (1,1,[]) diff
--     isInContext num = not $ all isBoth $ catMaybes [ IM.lookup i lnmap | i <- [(num - nctx)..(num + nctx)] ]
--     ctxmap = IM.foldlWithKey (\res n dv -> if isInContext n then IM.insert n dv res else res) IM.empty lnmap
--
-- printCtx [] = []
-- printCtx grp@((Both (n1,_) (n2,ln)):_) = (grpcaption ++ hdln):tllns
--   where
--     (len1, len2) = (length $ filter notSecond grp, length $ filter notFirst grp)
--     diffln dv = case dv of { Both(_,ln) _ -> ' ':ln; First(_,ln) -> '-':ln; Second(_,ln) -> '+':ln }
--     (hdln : tllns) = map diffln grp
--     grpcaption = printf "@@ -%d,%d +%d,%d @@ " n1 len1 n2 len2
--
-- parseConfig dat = reverse $ snd $ L.foldl' iter ("",[]) $ map (words . trim '[' ']' . takeWhile (/= '#')) $ lines dat
--   where
--     trim fc lc str = if head str == fc && last str == lc then (init $ tail str) else str
--     iter s@(pre, res) [] = s
--     iter (_, res)   [section] = (section, res)
--     iter (pre, res) [section, subsect] = ((section ++ "." ++ trim '"' '"' subsect), res)
--     iter (pre, res) (key:"=":val) = (pre, ((pre ++ "." ++ key, unwords val):res))
--     iter _ ln = error $ "config parsing error at : " ++ unwords ln
--
-- readConfig path = doesFileExist path >>= bool (parseConfig <$> readFile path) (pure [])
--
-- lookupConfigs :: String -> [[(String, String)]] -> Maybe String
-- lookupConfigs key = listToMaybe . catMaybes . map (L.lookup key)

objectParser :: Parser SHAHash'
objectParser = argument (eitherReader readSHA') (metavar "<object>")

data CatCommand =
    CatObjectType
  | CatObjectSize
  | CatPrettyBlob
  | CatCheckObjectExists
  | CatTyped BlobType
  deriving (Eq, Ord, Show)

catCommandParser :: Parser CatCommand
catCommandParser =
      flag' CatObjectType (short 't')
  <|> flag' CatObjectSize (short 's')
  <|> flag' CatPrettyBlob
        (short 'p' <>
         help "Pretty-print blob contents based on its type.")
  <|> flag' CatCheckObjectExists
        (short 'e' <>
         help "Don't print anything. Exit with zero status if object exists and non-zero status otherwise.")
  <|> CatTyped <$> blobTypeArgumentParser

data GitCommand =
    CatFile CatCommand SHAHash'
--  | VerifyPack

gitCommandParser :: Parser GitCommand
gitCommandParser =
  hsubparser
    (command "cat-file"
      (info
         (CatFile <$> catCommandParser <*> objectParser)
         (fullDesc <>
          header "Provide content or type and size information for repository objects")))

data GitConfig = GitConfig
  { cfgCommand :: GitCommand
  }

gitConfigParser :: Parser GitConfig
gitConfigParser = GitConfig
  <$> gitCommandParser

progInfo :: ParserInfo GitConfig
progInfo = info
  (helper <*> gitConfigParser)
  (fullDesc <> header "Haskell reimplementation of git")

runGit
  :: FilePath -- ^ Current directory
  -> [String] -- ^ Command-line arguments
  -> IO ()
runGit currentDirectory args = do
  GitConfig{cfgCommand} <- handleParseResult $ execParserPure defaultPrefs progInfo args

  outtty <- hIsTerminalDevice stdout
  let colPutStrLn color = if outtty then colorPutStrLn color else putStrLn

  -- search for a .git directory:
  gitdir <- findRepoDirectory currentDirectory
  let workdir = takeDirectory $ unRepoDirectory gitdir

  hasindex <- doesFileExist $ gitdir ++ "/index"
  index <- if hasindex then parseIndex <$> BL.readFile (gitdir ++ "/index") else pure []
  let indexByPath = M.fromList $ map (\ie -> (indFName ie, ie)) index

  -- read configs
  localconf <- readConfig (gitdir </> "config")
  userconf <- readConfig =<< ((</> ".gitconfig") <$> getHomeDirectory)

  -- find pack files and load them
  idxfiles <- filter (L.isSuffixOf ".idx") <$> getDirectoryContents (gitdir </> "objects" </> "pack")
  idxmaps <- zip idxfiles <$> forM idxfiles (parseIdxFile_v2 . ((gitdir ++ "/objects/pack/") ++))

  -- .gitignore
  let gitignpath = (workdir </> ".gitignore")
  gitignore <- (bool (map Glob.compile <$> lines <$> readFile gitignpath) (pure [])) =<< (doesFileExist gitignpath)

  let lc = 7  -- longest collision, TODO

  case cfgCommand of
    CatFile cmd hash -> do
      (blobtype, bloblen, blob) <- loadBlob gitdir idxmaps hash
      case cmd of
        CatObjectType -> putStrLn $ blobtype ++ "\n"
        CatObjectSize -> putStrLn $ show bloblen
        CatPrettyBlob -> putStr $ maybeOr "bad file" $ lookup blobtype
            [ ("blob", BLU.toString blob)
            , ("commit", BLU.toString blob)
            , ("tree", prettyTreeObject $ parseTreeObject blob)
            ]
        CatCheckObjectExists -> putStrLn "Not implemented yet" *> exitFailure
        CatTyped typ ->
          putStr $
            case typ of
              BlobCommit -> BLU.toString blob
              BlobTree   -> prettyTreeObject $ parseTreeObject blob
              BlobObject -> BLU.toString blob
              BlobTag    -> "Not implemented yet"

  -- case args of
  --   ["cat-file", opt, hash] -> do
  --     (blobtype, bloblen, blob) <- loadBlob gitdir idxmaps (readSHA hash)
  --     putStr $ maybeOr "Usage: omit cat-file [-t|-s|-p] <hash>" $ lookup opt
  --       [ ("-t", blobtype ++ "\n")
  --       , ("-s", show bloblen ++ "\n")
  --       , ("-p", maybeOr "bad file" $ lookup blobtype
  --           [ ("blob", BLU.toString blob)
  --           , ("commit", BLU.toString blob)
  --           , ("tree", prettyTreeObject $ parseTreeObject blob)
  --           ])
  --
  --       , ("blob", BLU.toString blob)
  --       , ("tree", prettyTreeObject $ parseTreeObject blob)
  --       , ("commit", BLU.toString blob)
  --       ]
  --
  --   ("verify-pack":argv') -> do
  --     let (verbose, packfile) = ("-v" `elem` argv', last argv')
  --     let verifyPack = do
  --             offmap <- parseIdxFile_v2 $ replaceExtension packfile "idx"
  --             let printHash (hsh, (off, sz, crc32)) =
  --                     putStrLn $ L.intercalate " " [showSHA hsh, show sz, show off]
  --             when verbose $ forM_ (M.toList offmap) printHash
  --             offmap `seq` pure ()
  --     verifyPack `Exc.catch` (\(e :: Exc.SomeException) -> when verbose (hPrint stderr e) >> exitFailure)
  --
  --   ("ls-files":argv') -> mapM_ (putStrLn . indFName) index
  --
  --   ["status"] -> do
  --     workfiles   <- S.fromList <$> fsTreeFlatten "" <$> fsTreeFromDir workdir "" gitignore
  --     headTreeSHA <- getHeadTree gitdir idxmaps
  --     headtree    <- loadTree gitdir idxmaps (readSHA headTreeSHA) ""
  --     let indfiles = map indFName index
  --         untracked = workfiles `S.difference` (S.fromList indfiles)
  --
  --     let isFileStaged ie fname = do
  --             st <- getFileStatus (workdir </> fname)
  --             let ctime = todFromPosix $ statusChangeTimeHiRes st
  --                 mtime = todFromPosix $ modificationTimeHiRes st
  --             pure (ctime == indCTime ie && mtime == indMTime ie)
  --     let sortTracked (new, modified, staged, deleted) fname = do
  --           exists <- doesFileExist (workdir </> fname)
  --           if not exists then pure (new, modified, staged, fname:deleted)  -- deleted
  --           else do
  --             case hashFromGitTree (splitDirectories fname) headtree of
  --               Nothing -> pure (fname:new, modified, staged, deleted) -- new
  --               Just headsha -> do
  --                 inindex <- isFileStaged (indexByPath M.! fname) fname
  --                 if inindex then
  --                   if (indSHA (indexByPath M.! fname) /= headsha)
  --                   then pure (new, modified, fname:staged, deleted) -- staged
  --                   else pure (new, modified, staged, deleted)     -- already committed
  --                 else pure (new, fname:modified, staged, deleted) -- modified
  --
  --     (new, modified, staged, deleted) <- foldM sortTracked ([], [], [], []) indfiles
  --
  --     let printFList col = mapM_ (colPutStrLn col . ('\t':))
  --     unless (L.null new) $ putStrLn "New files to be commited:" >> printFList Green new
  --     unless (L.null staged) $ putStrLn "Changes to be committed:" >> printFList Green staged
  --     unless (L.null modified) $ putStrLn "Changes not staged for commit:" >> printFList Red modified
  --     unless (L.null deleted) $ putStrLn "Deleted files:" >> printFList Red deleted
  --     unless (S.null untracked) $ putStrLn "Untracked files:" >> printFList Red (S.toAscList untracked)
  --
  --   ("config":argv') -> mapM_ (\(k, v) -> putStrLn $ k ++ "=" ++ v) localconf
  --
  --   ("log":[]) -> do
  --     let printCommit commit = do
  --             ("commit", _, blob) <- loadBlob gitdir idxmaps (readSHA commit)
  --             let (commMeta, commMsg) = parseCommitObject blob
  --             let (cmTZ : cmEpoch : cmAuthor) =
  --                     reverse $ words $ maybeOr "No commit author" $ M.lookup "author" commMeta
  --             colPutStrLn Yellow $ "commit " ++ commit
  --             putStrLn $ "Author:\t" ++ unwords (reverse $ cmAuthor)
  --             putStrLn $ "Date:\t" ++ show (TOD (read cmEpoch) 0)
  --             mapM_ (putStrLn . ("    " ++)) commMsg
  --             putStrLn ""
  --             let cmPar = M.lookup "parent" commMeta
  --             when (isJust cmPar) $ let Just parent = cmPar in printCommit parent
  --
  --     getHeadSHA gitdir >>= printCommit
  --
  --   ("diff":argv') -> do
  --     case argv' of
  --       [] -> forM_ index $ \ie -> do
  --               let (fname, stageSHA) = (indFName ie, (showSHA $ indSHA ie))
  --               workdirBlob <- BL.readFile (workdir </> fname)
  --               let fileSHA = show (SHA.sha1 $ blobify "blob" workdirBlob)
  --               when (fileSHA /= stageSHA) $ do
  --                 let workdirLines = map BLU.toString $ BLU.lines workdirBlob
  --                 ("blob", _, stagedBlob) <- loadBlob gitdir idxmaps (readSHA stageSHA)
  --                 let stagedLines = map BLU.toString $ BLU.lines stagedBlob
  --                     diffcap = [ printf "diff --git a/%s b/%s" fname fname,
  --                         printf "index %s..%s %o" (take lc stageSHA) (take lc fileSHA) (indMode ie),
  --                         printf "--- a/%s\n+++ b/%s" fname fname ]
  --                     prettyDiff = concat . map printCtx . contextDiff 3
  --                     colDiffprint ln@(c:_) = (maybe putStrLn colPutStrLn $ L.lookup c [('+',Green), ('-',Red), ('@',Cyan)]) ln
  --                 mapM_ putStrLn diffcap
  --                 mapM_ colDiffprint $ prettyDiff $ Diff.getDiff stagedLines workdirLines
  --
  --       _ -> hPutStrLn stderr $ "Usage: omit diff"
  --
  --   ("add":argv') -> do
  --     let iterargv pathidx rpath = do
  --           path <- Dir.canonicalizePath (currentDirectory </> rpath)
  --           s <- getFileStatus path
  --           (blob, mod) <- case s of
  --             _ | isRegularFile s  -> (, bool 0o100755 0o100644 (fileMode s `testBit` 6)) <$> BL.readFile path
  --             _ | Posix.isSymbolicLink s -> (, 0o120000) <$> BLU.fromString <$> Posix.readSymbolicLink path
  --             _ -> error ("not a valid file to add: " ++ rpath)
  --           sha <- writeBlob gitdir idxmaps "blob" blob
  --           let fname = makeRelative workdir path
  --               ie = IndexEntry (todFromPosix $ statusChangeTimeHiRes s) (todFromPosix $ modificationTimeHiRes s)
  --                     (int$deviceID s) (int$fileID s) mod (int$fileOwner s) (int$fileGroup s) (int $ fileSize s)
  --                     sha (0x7ff .&. int (B.length $ BU.fromString fname)) fname
  --           pure $ M.insert fname ie pathidx
  --
  --     pathidx <- foldM iterargv indexByPath argv'
  --     let (omit_index, indpath, indbackup) = (gitdir </> "omit_index", gitdir </> "index", gitdir </> "index.old")
  --     BL.writeFile omit_index $ dumpIndex pathidx
  --     doesFileExist indpath >>= (flip when (Dir.renameFile indpath indbackup))
  --     Dir.renameFile omit_index indpath
  --
  --   ("write-tree":argv') -> do
  --     treesha <- writeTree gitdir idxmaps =<< makeTreeFromIndex workdir indexByPath
  --     putStrLn $ showSHA treesha
  --
  --   ("checkout":argv') -> do
  --     let (opts, paths) = Arr.second (dropWhile (== "--")) $ L.break (== "--") argv'
  --     if L.null paths then error "TODO: checkout <branch> not implemented"
  --     else error "TODO: checkout -- <paths> not implemneted yet"
  --
  --   ("commit":argv') -> do
  --     (prevcommit, reffile) <- (,) <$> getHeadSHA gitdir <*> getHeadRef gitdir
  --     prevtree <- getHeadTree gitdir idxmaps
  --     treesha  <- writeTree gitdir idxmaps =<< makeTreeFromIndex workdir indexByPath
  --     when (treesha == readSHA prevtree) $ error "no changes to commit"
  --
  --     editor  <- fromMaybe (fromMaybe "vi" $ lookupConfigs "core.editor" [localconf, userconf]) <$> lookupEnv "EDITOR"
  --     Proc.runCommand (editor ++ " " ++ (gitdir </> "COMMIT_EDITMSG")) >>= Proc.waitForProcess
  --     editmsg <- readFile (gitdir </> "COMMIT_EDITMSG")
  --     let commMsg = unlines $ filter (not.null) $ map (dropWhile isSpace . takeWhile (/= '#')) $ lines editmsg
  --     when (null commMsg) $ error "no commit message"
  --
  --     let author = maybeOr "No user.name configured" $ lookupConfigs "user.name" [localconf, userconf]
  --     let email = maybeOr "No user.email configured" $ lookupConfigs "user.email" [localconf, userconf]
  --     TOD epoch _ <- getClockTime
  --     tzoffset <- timeZoneOffsetString <$> getCurrentTimeZone
  --     let cmAuthor = unwords [author, "<" ++ email ++ ">", show epoch, tzoffset]
  --
  --     let commMeta = [("tree", showSHA treesha),("parent", prevcommit),("author", cmAuthor),("committer", cmAuthor)]
  --     commit <- showSHA <$> commitTree gitdir commMeta commMsg
  --     writeFile (gitdir </> "omit_ref") commit
  --     Dir.renameFile (gitdir </> "omit_ref") (gitdir </> reffile)
  --     putStrLn commit
  --
  --   _ -> error "Usage: omit [cat-file|verify-pack|ls-files|log|diff|add|commit]"
