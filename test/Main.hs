module Main (main) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Map ((!))
import qualified Data.Map as Map (Map, empty, foldrWithKey, fromList, insert, insertWith, keys)
import GHC.IO.Handle (hGetContents)
import Lib (CairoFilePath, CompiledFilePath, ResultFilePath, SmtLibFilePath, StructuredFilePath (base), mkCairoFilePath, mkCompiledFilePath, mkResultFilePath, mkSmtLibFilePath, relativePath)
import SMTSettings (SMT (..), SmtLibCommand (..), SmtTag (..), allSmts)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (dropFileName, takeDirectory, takeFileName, (</>))
import System.Process (CreateProcess (cwd, std_err, std_out), StdStream (CreatePipe), callProcess, createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec (Spec, SpecWith, afterAll_, beforeAll, describe, hspec, it, parallel, shouldBe)
import Wheels (millisToMicros)

horusCompileCmd :: String
horusCompileCmd = "horus-compile"

horusCheckCmd :: String
horusCheckCmd = "horus-check"

benchmarkerRoot :: String
benchmarkerRoot = "test/benchmark_src/"

benchmarker :: String
benchmarker = "test/benchmark_src/build/bin/benchmark"

lake :: String
lake = "lake"

generalSegmentsDir :: FilePath
generalSegmentsDir = "generalSegments"

resultsDir :: SmtLibFilePath -> FilePath
resultsDir root = takeDirectory (base root) ++ "Res"

data ToolchainPart
  = Compiler
  | Segmentizer
  | Preprocessor
  | Verifier
  deriving (Enum, Eq, Ord)

toolchainSequence :: ToolchainPart -> ToolchainPart -> [ToolchainPart]
toolchainSequence begin end = [tool | tool <- [Compiler ..], begin <= tool && tool <= end]

type SmtTagged a = Map.Map SmtTag a

data TestState = TestState
  { ts_toolsToRun :: [ToolchainPart]
  , ts_outDir :: FilePath
  , ts_timeoutInMillis :: Int
  , ts_smts :: [SmtTag]
  , ts_seed :: Int
  , ts_compileBmarker :: Bool
  }

defaultTestState :: TestState
defaultTestState =
  TestState
    { ts_toolsToRun = toolchainSequence Compiler Verifier
    , ts_outDir = "out"
    , ts_timeoutInMillis = 5000
    , ts_smts = allSmts
    , ts_seed = 0
    , ts_compileBmarker = True
    }

type TestIO a = ReaderT TestState IO a

isToolRequested :: TestState -> ToolchainPart -> Bool
isToolRequested st part = part `elem` ts_toolsToRun st

smtRoot :: FilePath -> SmtTag -> FilePath
smtRoot file smt = (</> name smt) . dropFileName . takeDirectory $ file

smtFileName :: SmtLibFilePath -> SmtTag -> SmtLibFilePath
smtFileName file smt = mkSmtLibFilePath $ smtRoot filePath smt </> takeFileName filePath
 where
  filePath = relativePath file

compileCairo :: CairoFilePath -> TestIO CompiledFilePath
compileCairo file = do
  st <- ask
  let compiled :: CompiledFilePath = mkCompiledFilePath (ts_outDir st) file
  liftIO . createDirectoryIfMissing True . dropFileName . base $ compiled
  when (isToolRequested st Compiler) $
    liftIO $
      callProcess horusCompileCmd [relativePath file, "--output", relativePath compiled]
  return compiled

makeGeneralSegments :: CompiledFilePath -> TestIO [SmtLibFilePath]
makeGeneralSegments file =
  let generalSegRoot = dropFileName (base file) </> generalSegmentsDir
   in do
        st <- ask
        when (isToolRequested st Segmentizer) $
          liftIO $
            callProcess horusCheckCmd [relativePath file, generalSegRoot]
        segmentFiles <- liftIO $ listDirectory generalSegRoot
        return $ map (mkSmtLibFilePath . (</>) generalSegRoot) segmentFiles

makeSmtSpecificSegment :: SmtTag -> Int -> String -> String
makeSmtSpecificSegment smt seed input =
  concat [flatten . flip prefix seed $ smt, input, "\n", flatten $ suffix smt]
 where
  flatten :: [SmtLibCommand] -> String
  flatten = concatMap ((++ "\n") . show)

makeSmtSpecificSegments :: [SmtLibFilePath] -> TestIO (SmtTagged [SmtLibFilePath])
makeSmtSpecificSegments generalSegments = do
  st@(TestState _ _ _ smts seed _) <- ask
  let newRoots = Map.fromList [(smt, smtRoot (base $ head generalSegments) smt) | smt <- smts]
  liftIO $ mapM_ (createDirectoryIfMissing True) newRoots
  foldM
    ( \acc smt ->
        let segFileNames = map (`smtFileName` smt) generalSegments
         in do
              when (isToolRequested st Preprocessor) $
                forM_ generalSegments $ \segment -> do
                  contents <- liftIO . readFile $ relativePath segment
                  liftIO $
                    writeFile (relativePath (smtFileName segment smt)) $
                      makeSmtSpecificSegment smt seed contents
              return (Map.insert smt segFileNames acc)
    )
    Map.empty
    smts

invokeSmts :: SmtTagged [SmtLibFilePath] -> TestIO (SmtTagged [ResultFilePath])
invokeSmts smtToSegs = do
  (TestState _ _ timeoutInMillis _ seed _) <- ask
  foldM
    ( \res smt -> do
        foldM
          ( \res' segment -> do
              (_, Just out, _, h) <-
                liftIO $
                  createProcess
                    ( proc (runCmd smt) $
                        addtnlArgs smt seed ++ [relativePath segment]
                    )
                      { std_out = CreatePipe
                      , std_err = CreatePipe
                      }
              mbExitCode <- liftIO $ timeout (millisToMicros timeoutInMillis) $ waitForProcess h
              let outDir = resultsDir segment
              let outPath = mkResultFilePath $ outDir </> takeFileName (relativePath segment)
              liftIO $ createDirectoryIfMissing True outDir
              let newMap = Map.insertWith (++) smt [outPath] res'
              case mbExitCode of
                Nothing ->
                  liftIO $
                    writeFile (relativePath outPath) ""
                      >> terminateProcess h
                      >> return newMap
                Just _ ->
                  liftIO $
                    hGetContents out
                      >>= writeFile (relativePath outPath)
                      >> return newMap
          )
          res
          (smtToSegs ! smt)
    )
    Map.empty
    $ Map.keys smtToSegs

processResults :: SmtTagged [ResultFilePath] -> IO (SmtTagged [ResultFilePath])
processResults smtToResults =
  let args =
        Map.foldrWithKey
          ( \smt results acc ->
              acc ++ [["--file=" ++ relativePath res, "--smt=" ++ name smt] | res <- results]
          )
          []
          smtToResults
   in forM_ args (callProcess benchmarker) >> return smtToResults

postprocessedResult :: String -> String
postprocessedResult result =
  if null result
    then "X"
    else case lines result of
      (smtResult : smtTimeLine : _) ->
        concat [smtResult, " ", last $ words smtTimeLine, "s"]
      _ -> fail "Processed SMT result has incorrect format"

postprocessResults :: SmtTagged [ResultFilePath] -> IO (SmtTagged [String])
postprocessResults smtToResults =
  foldM
    ( \res smt -> do
        str <-
          foldM
            ( \acc path -> do
                result <- readFile $ relativePath path
                return $ acc ++ [concat [name smt, ": ", postprocessedResult result]]
            )
            []
            (smtToResults ! smt)
        return $ Map.insert smt str res
    )
    Map.empty
    $ Map.keys smtToResults

modelTest :: FilePath -> SpecWith TestState
modelTest file =
  parallel $
    describe file $ do
      it "should be unsat" $ \st -> do
        _ <-
          flip runReaderT st $
            compileCairo (mkCairoFilePath file)
              >>= makeGeneralSegments
              >>= makeSmtSpecificSegments
              >>= invokeSmts
              >>= liftIO . processResults
              >>= liftIO . postprocessResults
              >>= liftIO . putStrLn . ((file ++ " ") ++) . show
        (1 :: Int) `shouldBe` 1

makeInitEnv :: TestState -> IO TestState
makeInitEnv st = do
  when (ts_compileBmarker st) $
    -- StdOut and StdErr stolen and ignored on purpose in order to discard compiler output.
    do
      (_, _, _, h) <-
        createProcess
          (proc lake ["build"])
            { cwd = Just benchmarkerRoot
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
      _ <- waitForProcess h
      return ()
  createDirectoryIfMissing True $ ts_outDir st
  return st

spec :: TestState -> Spec
spec st = beforeAll (makeInitEnv st) $
  afterAll_ (return ()) $ do
    describe "Cairo semantic models" $ do
      modelTest "tests/resources/golden/func_if.cairo"

-- modelTest "tests/resources/golden/func_peano_prod.cairo"
-- modelTest "tests/resources/golden/func_id.cairo"
-- modelTest "tests/resources/golden/func_mul3.cairo"
-- modelTest "tests/resources/golden/func_multiple_ret.cairo"
-- modelTest "tests/resources/golden/func_pred.cairo"
-- modelTest "tests/resources/golden/func_add_rec.cairo"

main :: IO ()
main = do
  hspec $ spec defaultTestState
