#!/usr/bin/env runhaskell

import System.Directory
import System.FilePath
import System.Environment
import Control.Monad
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- ======================
-- Конфигурация
-- ======================

data Config = Config
    { onlyFiles :: Bool
    , onlyDirs  :: Bool
    , exts      :: [String]
    , maxDepth  :: Maybe Int
    , countOnly :: Bool
    }

defaultConfig :: Config
defaultConfig = Config False False [] Nothing False

-- ======================
-- Статистика
-- ======================

data Stats = Stats
    { filesCount :: Int
    , dirsCount  :: Int
    }

emptyStats :: Stats
emptyStats = Stats 0 0

mergeStats :: Stats -> Stats -> Stats
mergeStats (Stats f1 d1) (Stats f2 d2) =
    Stats (f1 + f2) (d1 + d2)

-- ======================
-- CLI-парсер
-- ======================

parseArgs :: [String] -> (Config, FilePath)
parseArgs args =
    let cfg = foldl parseFlag defaultConfig args
        path = last args
    in (cfg, path)

parseFlag :: Config -> String -> Config
parseFlag cfg arg
    | arg == "--files"     = cfg { onlyFiles = True }
    | arg == "--dirs"      = cfg { onlyDirs  = True }
    | arg == "--count"     = cfg { countOnly = True }
    | "--ext=" `isPrefix` arg =
        cfg { exts = split ',' (drop 6 arg) }
    | "--max-depth=" `isPrefix` arg =
        cfg { maxDepth = readMaybe (drop 12 arg) }
    | otherwise = cfg

isPrefix :: String -> String -> Bool
isPrefix p s = take (length p) s == p

split :: Char -> String -> [String]
split _ "" = []
split c s =
    let (a, b) = break (== c) s
    in a : case b of
        []      -> []
        (_:xs)  -> split c xs

-- ======================
-- ANSI цвета
-- ======================

blue, green, yellow, reset :: String
blue   = "\ESC[34m"
green  = "\ESC[32m"
yellow = "\ESC[33m"
reset  = "\ESC[0m"

-- ======================
-- Сканирование
-- ======================

scanDir :: Config -> Int -> FilePath -> IO Stats
scanDir cfg depth path = do
    stop <- case maxDepth cfg of
        Just m  -> pure (depth > m)
        Nothing -> pure False

    if stop then pure emptyStats else do
        contents <- listDirectory path
        foldM (scanEntry cfg depth path) emptyStats (sort contents)

scanEntry :: Config -> Int -> FilePath -> Stats -> FilePath -> IO Stats
scanEntry cfg depth base stats name = do
    let full = base </> name
    isDir <- doesDirectoryExist full
    if isDir
        then do
            when (not (onlyFiles cfg) && not (countOnly cfg)) $
                putStrLn $ blue ++ "[DIR]  " ++ full ++ reset
            subStats <- scanDir cfg (depth + 1) full
            pure $ mergeStats stats (subStats `mergeStats` Stats 0 1)
        else do
            let okExt =
                    null (exts cfg)
                 || any (`isSuffixOf` name) (map ("." ++) (exts cfg))
            if okExt
                then do
                    when (not (onlyDirs cfg) && not (countOnly cfg)) $
                        putStrLn $ green ++ "[FILE] " ++ full ++ reset
                    pure $ mergeStats stats (Stats 1 0)
                else pure stats

-- ======================
-- Main
-- ======================

main :: IO ()
main = do
    args <- getArgs
    when (null args) $
        error "Usage: color.hs [flags] <directory>"

    let (cfg, path) = parseArgs args
    stats <- scanDir cfg 0 path

    when (countOnly cfg) $ do
        putStrLn "\n--- Stats ---"
        putStrLn $ yellow ++ "Files: " ++ show (filesCount stats)
        putStrLn $ yellow ++ "Dirs:  " ++ show (dirsCount stats)
        putStrLn reset