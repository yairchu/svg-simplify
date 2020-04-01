{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens.Operators
import Data.Version (showVersion)
import Graphics.Svg (loadSvgFile)
import qualified Options.Applicative as O
import Paths_svg_simplify (version)
import Simplify (simplifyDocument)
import System.Exit (exitFailure, exitSuccess)
import Text.Pretty.Simple (pPrint)

data Options
  = Options
      { filename :: FilePath
      }

data CmdArgs = CmdVersion | CmdOptions Options

parser :: O.ParserInfo CmdArgs
parser =
  O.info
    (O.helper <*> (version <|> (optsParser <&> CmdOptions)))
    ( O.fullDesc
        <> O.progDesc "Modify SVG files to use more basic features but look the same"
        <> O.header "svg-simplify - improve SVG files compatibilty"
    )
  where
    version =
      O.flag'
        CmdVersion
        (O.long "version" <> O.short 'v' <> O.help "Print the version and quit")
    optsParser =
      Options
        <$> O.argument O.str (O.metavar "FILE")

getOpts :: IO Options
getOpts =
  O.execParser parser
    >>= \case
      CmdVersion -> putStrLn ("svg-simplify version " ++ showVersion version) >> exitSuccess
      CmdOptions o -> return o

main :: IO ()
main =
  do
    Options filename <- getOpts
    loadSvgFile filename
      >>= \case
        Nothing -> putStrLn ("Failed to parse SVG file: " <> filename) >> exitFailure
        Just svg -> simplifyDocument svg & pPrint
