{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Control.Monad (forM_)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import NotSoSimpleGenericParser
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Pattern
    = AnyChar
    | Literal ByteString
    | StartOfLine Pattern
    | EndOfLine Pattern
    | CharClass [Char] Bool
    | Star Pattern
    | Plus Pattern
    | Optional Pattern
    | Alt [Pattern]
    deriving (Show)

type GrepParser = Parser ByteString

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: simple-grep PATTERN [FILE...]"
            exitFailure
        (patternStr : files) -> do
            -- Parse the pattern
            case parsePattern patternStr of
                Left err -> do
                    putStrLn $ "Error parsing pattern: " ++ err
                    exitFailure
                Right pat -> do
                    -- Process files or stdin
                    if null files
                        then do
                            content <- BS.getContents
                            let matches = filter (matchPattern pat) (BS.lines content)
                            mapM_ BS.putStrLn matches
                        else do
                            forM_ files $ \file -> do
                                content <- BS.readFile file
                                let matches = filter (matchPattern pat) (BS.lines content)
                                if length files > 1
                                    then forM_ matches $ \line ->
                                        BS.putStrLn $ BS.concat [BS.pack file, BS.pack ": ", line]
                                    else mapM_ BS.putStrLn matches


-- Parse a regex pattern string into our Pattern type
parsePattern :: String -> Either String Pattern
parsePattern str = 
  case parse patternParser str of
    Success (pat, _) -> Right pat
    Failure (err, _) -> Left err

literalParser :: Parser String Pattern
literalParser = Literal . BS.pack <$> some (noneOf "")

patternParser :: Parser String Pattern
patternParser = altParser

-- parseStar = Star $

altParser :: Parser String Pattern
altParser = Alt <$> sepBy1 sequenceParser (char '|')

sequenceParser :: Parser String Pattern
sequenceParser = do
    startAnchor <- (char '^' *> pure True) <|> pure False

    pat <- basicPatternParser

    endAnchor <- (char '$' *> pure True) <|> pure False

    let anchored = if startAnchor then StartOfLine pat else pat
    return $ if endAnchor then EndOfLine anchored else anchored

basicPatternParser :: Parser String Pattern
basicPatternParser =
    choice
        [ char '.' *> pure AnyChar
        , Literal . BS.pack <$> some (noneOf "^$.|()")
        ]

matchPattern :: Pattern -> ByteString -> Bool
matchPattern AnyChar line = not (BS.null line)
matchPattern (Literal pat) line = pat `BS.isInfixOf` line
matchPattern (StartOfLine pat) line =
    matchPattern pat prefix
  where
    prefix = BS.take 1 line
matchPattern (EndOfLine pat) line =
    matchPattern pat suffix
  where
    suffix = BS.drop (BS.length line - 1) line
matchPattern (Alt patterns) line =
    any (`matchPattern` line) patterns
matchPattern _ _ = False

grepWithPattern :: Pattern -> ByteString -> [ByteString]
grepWithPattern pat content =
    filter (matchPattern pat) (BS.lines content)
