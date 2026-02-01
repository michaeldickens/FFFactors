{- |
Module      : ManageFrench
Description : Functions to manipulate Ken French data and convert other data sets into French format

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2021-03-26

-}

module ManageFrench where

import Control.Monad (when)
import Data.Foldable (for_)
-- import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Debug.Trace
import Text.Printf
import qualified Text.Read as Read
import qualified Data.Text.IO as Text
import System.Directory
import System.Environment


{-
data Weighting = VW | EW deriving (Eq)

-- | Read in a database from the given file path, returning a list of lists. The
-- first list is the header line containing the names of all the fields.
readDB :: Char -> FilePath -> IO [[Text.Text]]
readDB separator filename =
  Text.readFile filename >>=
  (return . map (map Text.strip . Text.split (==separator)) . Text.lines)


readCSVDB = readDB ','


getColIndex :: [Text.Text] -> String -> Int
getColIndex header colName = fromJust $ elemIndex (Text.pack colName) header


-- | Make a de-duped list of all values that appear in a column. We use this to
-- create lists of all dates / factors.
uniqueColValues :: [[Text.Text]] -> Int -> [Text.Text]
uniqueColValues rows colIndex =
  sort $ Set.toList $ foldl (\accum row -> Set.insert (row !! colIndex) accum) Set.empty rows


-- | Convert JKP data into the same format as French data.
jkpToFrench :: IO ()
jkpToFrench = do
  allCountryRows <- readCSVDB "resources/JKP/hml_pfs.csv"
  marketRows <- readCSVDB "resources/JKP/market_returns.csv"

  let weighting = EW

  let factorsHeader = head allCountryRows
  let marketHeader = head marketRows

  let countryCol = getColIndex factorsHeader "excntry"
  let dateCol = getColIndex factorsHeader "eom"
  let characteristicCol = getColIndex factorsHeader "characteristic"
  let retCol = getColIndex factorsHeader $ if weighting == VW then "ret_vw" else "ret_ew"

  let targetCountry = "USA"

  -- This lazily evaluates, so we don't have to read the entire file into
  -- memory, only the lines that match the filter.
  let factorRows = filter (\row -> row !! countryCol == targetCountry) $ tail allCountryRows

  let numberToPercentage text = Text.pack $ printf "%.6f" $ 100 * (Read.read $ Text.unpack text :: Double)

  -- Construct a map from (date, characteristic) to return
  let factorsMap = foldl (
        \accum row ->
          Map.insert (row !! dateCol, row !! characteristicCol)
          (numberToPercentage (row !! retCol))
          accum)
        (Map.empty :: Map.HashMap (Text.Text, Text.Text) Text.Text) factorRows

  -- Construct a map from date to return
  let marketMap = foldl (
        \accum row ->
          Map.insert (row !! getColIndex marketHeader "eom")
          (numberToPercentage (row !! getColIndex marketHeader (if weighting == VW then "mkt_vw" else "mkt_ew")))
          accum
        )
        (Map.empty :: Map.HashMap Text.Text Text.Text)
        $ filter (\row -> row !! (getColIndex marketHeader "excntry") == targetCountry)
        $ tail marketRows

  let dates = uniqueColValues factorRows dateCol
  let factors = uniqueColValues factorRows characteristicCol

  putStr "%Y%m%d,mkt,"
  putStrLn $ intercalate "," $ map Text.unpack factors
  sequence $ map (
    \date -> do
      let rets = map (\factor -> fromMaybe ""
                       $ Map.lookup (date, factor) factorsMap) factors
      let marketRet = fromMaybe "" $ Map.lookup date marketMap
      Text.putStrLn $ Text.intercalate "," $ date:marketRet:rets
    ) dates
  return ()


-- | Convert Chen & Zimmerman data into the same format as French data.
chenToFrench :: IO ()
chenToFrench = do
  rowsWithHeader <- readCSVDB "resources/Chen/CheckPredictorPorts_Deciles.csv"

  let header = head rowsWithHeader
  let decileRows = tail rowsWithHeader

  let factorsMap = foldl (
        \accum row ->
          Map.insert (row !! getColIndex header "date", row !! getColIndex header "signalname")
          (row !! getColIndex header "ret")
          accum
        )
        (Map.empty :: Map.HashMap (Text.Text, Text.Text) Text.Text)
        decileRows

  -- Note: What Chen calls a "signal" is the same thing JKP calls a "characteristic"
  let dates = uniqueColValues decileRows $ getColIndex header "date"
  let factors = uniqueColValues decileRows $ getColIndex header "signalname"

  putStr "%Y-%m-%d,"
  putStrLn $ intercalate "," $ map Text.unpack factors
  sequence $ map (
    \date -> do
      let rets = map (\factor -> fromMaybe ""
                       $ Map.lookup (date, factor) factorsMap) factors
      Text.putStrLn $ Text.intercalate "," $ date:rets
    ) dates
  return ()
-}


-- | Split a Ken French CSV file into two CSV files, one for value-weighted
-- returns and one for equal-weighted returns, with the suffixes "Value_Wt" and
-- "Equal_Wt", respectively.
--
-- By default, files downloaded from the Ken French data library contain some
-- descriptive text and then four blocks of data: (1) value-weighted monthly,
-- (2) value-weighted annual, (3) equal-weighted monthly, (4) equal-weighted
-- annual. Throw away the descriptions and annual data, and put the two monthly
-- return series into their own files with just a header line and data lines.
splitFrenchFile :: FilePath -> Bool -> IO ()
splitFrenchFile infile quiet = do
  let fileWithoutExtension =
        case stripPrefix (reverse ".CSV") $ reverse infile of
          -- Remove the "Formed_on_" substring by converting to Text b/c
          -- Data.List doesn't have a splitOn function without installing an
          -- extra library
          Just s -> foldl1 (++) $ map Text.unpack
                    $ Text.splitOn (Text.pack "Formed_on_")
                    $ Text.pack $ reverse s
          Nothing -> error $ printf "Filename %s does not end in .CSV" infile

  -- Special case for factor files, which don't have separate sections for
  -- value-weight and equal-weight
  let isFactorFile = "Factor" `isInfixOf` fileWithoutExtension

  -- Special case for the Momentum Factor file, where the processing rules for
  -- factor files don't work because the spacing at the top of the file is
  -- different.
  let isMomentumFactor = fileWithoutExtension == "Momentum_Factor"

  let vwOutfile = fileWithoutExtension ++ "_Value_Wt.csv"
  let ewOutfile = fileWithoutExtension ++ "_Equal_Wt.csv"
  let factorOutfile = fileWithoutExtension ++ ".csv"

  exists <- doesFileExist infile
  when (not exists) $ error $ printf "File %s does not exist" infile

  contents <- Text.readFile infile

  -- Data chunks are separated by empty lines
  let chunks = Text.splitOn (Text.pack "\r\n\r\n") contents

  -- For factor files, the monthly data chunk has no identifiable header, but it
  -- is the 1st chunk that contains at least 120 lines (= 10 years).
  when isFactorFile $ do
    Text.writeFile factorOutfile $ head $ dropWhile (\chunk -> Text.count (Text.pack "\n") chunk < 120) chunks

  wroteVW <- newIORef False
  wroteEW <- newIORef False

  for_ chunks $ \chunk' -> do
    -- Skip any remaining empty lines
    let chunk = Text.dropWhile (`elem` "\r\n") chunk'

    let chunkHeader = Text.strip $ Text.takeWhile (/='\n') chunk
    let chunkRest = Text.strip $ Text.dropWhile (/='\n') chunk

    when (chunkHeader `elem` (map Text.pack ["Average Value Weighted Returns -- Monthly", "Average Value Weight Returns -- Monthly", "Value Weighted Returns -- Monthly", "Value Weight Returns -- Monthly"])) $ do
      writeIORef wroteVW True
      Text.writeFile vwOutfile chunkRest
    when (chunkHeader `elem` (map Text.pack ["Average Equal Weighted Returns -- Monthly", "Average Equal Weight Returns -- Monthly", "Equal Weighted Returns -- Monthly", "Equal Weight Returns -- Monthly"])) $ do
      writeIORef wroteEW True
      Text.writeFile ewOutfile chunkRest

  wroteVW' <- readIORef wroteVW
  when (not wroteVW' && not isFactorFile) $
    printf "ManageFrench.hs: error: could not find value-weighted returns in %s\n" infile

  wroteEW' <- readIORef wroteEW
  when (not wroteEW' && not isFactorFile) $
    printf "ManageFrench.hs: error: could not find equal-weighted returns in %s\n" infile


main :: IO ()
main = do
  argv <- getArgs
  let quiet = "-q" `elem` argv || "--quiet" `elem` argv
  let filename = head $ filter (\arg -> arg /= "-q" && arg /= "--quiet") argv
  splitFrenchFile filename quiet
