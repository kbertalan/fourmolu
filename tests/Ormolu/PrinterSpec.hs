{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.PrinterSpec (spec) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Ormolu
import Ormolu.Config
import Path
import Path.IO
import qualified System.FilePath as F
import Test.Hspec

spec :: Spec
spec = parallel $ do
  es <- runIO locateExamples
  let ormoluOpts =
        PrinterOpts
          { poIndentation = pure 2,
            poCommaStyle = pure Trailing,
            poIndentWheres = pure True,
            poRecordBraceSpace = pure True,
            poDiffFriendlyImportExport = pure False,
            poRespectful = pure False,
            poHaddockStyle = pure HaddockSingleLine,
            poNewlinesBetweenDecls = pure 1
          }
  sequence_ $ uncurry checkExample <$> [(ormoluOpts, "-out"), (defaultPrinterOpts, "-four-out")] <*> es <*> [True]

  let allOpts =
        [ opt
          | indentation <- pure <$> [2, 4],
            commaStyle <- enumOptions,
            indentWheres <- enumOptions,
            recordBraceSpace <- enumOptions,
            diffFriendlyImportExport <- enumOptions,
            respectful <- enumOptions,
            haddockStyle <- enumOptions,
            newlinesBetweenDecls <- pure <$> [1, 2],
            let opt =
                  PrinterOpts
                    { poIndentation = indentation,
                      poCommaStyle = commaStyle,
                      poIndentWheres = indentWheres,
                      poRecordBraceSpace = recordBraceSpace,
                      poDiffFriendlyImportExport = diffFriendlyImportExport,
                      poRespectful = respectful,
                      poHaddockStyle = haddockStyle,
                      poNewlinesBetweenDecls = newlinesBetweenDecls
                    },
            opt /= ormoluOpts,
            opt /= defaultPrinterOpts
        ]
      allOptsWithSuffixes = (\opt -> (opt, suffixForOpt opt)) <$> allOpts

  sequence_ $ uncurry checkExample <$> allOptsWithSuffixes <*> filter isComplex es <*> [False]

enumOptions :: (Monad m, Enum a, Bounded a) => [m a]
enumOptions = pure <$> enumFromTo minBound maxBound

suffixForOpt :: PrinterOptsTotal -> String
suffixForOpt PrinterOpts {..} =
  intercalate "-" $
    filter
      (not . null)
      [ "-i" <> show (runIdentity poIndentation),
        "cs" <> case runIdentity poCommaStyle of
          Trailing -> "t"
          Leading -> "l",
        if runIdentity poIndentWheres then "iw" else "",
        if runIdentity poRecordBraceSpace then "rbs" else "",
        if runIdentity poDiffFriendlyImportExport then "dfie" else "",
        if runIdentity poRespectful then "r" else "",
        "hs" <> case runIdentity poHaddockStyle of
          HaddockSingleLine -> "sl"
          HaddockMultiLine -> "ml",
        "nbd" <> show (runIdentity poNewlinesBetweenDecls),
        "out"
      ]

-- | Check a single given example.
checkExample :: PrinterOptsTotal -> String -> Path Rel File -> Bool -> Spec
checkExample po suffix srcPath' failIfExpectedMissing = do
  let srcPath = examplesDir </> srcPath'
      cfg = defaultConfig {cfgPrinterOpts = po}
  expectedOutputPath <- runIO $ deriveOutput suffix srcPath
  expectedOutputExists <- runIO $ doesFileExist expectedOutputPath
  testCaseName <- runIO $ deriveOutput suffix srcPath'
  when (failIfExpectedMissing || expectedOutputExists) $
    it (fromRelFile testCaseName ++ " works") . withNiceExceptions $ do
      -- 1. Given input snippet of source code parse it and pretty print it.
      -- 2. Parse the result of pretty-printing again and make sure that AST
      -- is the same as AST of the original snippet. (This happens in
      -- 'ormoluFile' automatically.)
      formatted0 <- ormoluFile cfg (fromRelFile srcPath)
      -- 3. Check the output against expected output. Thus all tests should
      -- include two files: input and expected output.
      -- T.writeFile (fromRelFile expectedOutputPath) formatted0
      expected <- (liftIO . T.readFile . fromRelFile) expectedOutputPath
      shouldMatch False formatted0 expected
      -- 4. Check that running the formatter on the output produces the same
      -- output again (the transformation is idempotent).
      formatted1 <- ormolu cfg "<formatted>" (T.unpack formatted0)
      shouldMatch True formatted1 formatted0

-- | Build list of examples for testing.
locateExamples :: IO [Path Rel File]
locateExamples =
  filter isInput . snd <$> listDirRecurRel examplesDir

-- | Does given path look like input path (as opposed to expected output
-- path)?
isInput :: Path Rel File -> Bool
isInput path =
  let s = fromRelFile path
      (s', exts) = F.splitExtensions s
   in exts == ".hs" && not ("-out" `isSuffixOf` s')

isComplex :: Path Rel File -> Bool
isComplex path =
  "complex/" `isPrefixOf` fromRelFile path

-- | For given path of input file return expected name of output.
deriveOutput :: String -> Path Rel File -> IO (Path Rel File)
deriveOutput suffix path =
  parseRelFile $
    F.addExtension (F.dropExtensions (fromRelFile path) ++ suffix) "hs"

-- | A version of 'shouldBe' that is specialized to comparing 'Text' values.
-- It also prints multi-line snippets in a more readable form.
shouldMatch :: Bool -> Text -> Text -> Expectation
shouldMatch idempotenceTest actual expected =
  when (actual /= expected) . expectationFailure $
    unlines
      [ ">>>>>>>>>>>>>>>>>>>>>> expected (" ++ pass ++ "):",
        T.unpack expected,
        ">>>>>>>>>>>>>>>>>>>>>> but got:",
        T.unpack actual
      ]
  where
    pass =
      if idempotenceTest
        then "idempotence pass"
        else "first pass"

examplesDir :: Path Rel Dir
examplesDir = $(mkRelDir "data/examples")

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.
withNiceExceptions ::
  -- | Action that may throw the exception
  Expectation ->
  Expectation
withNiceExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO ()
    h = expectationFailure . displayException
