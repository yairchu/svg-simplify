{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens.Operators
import Data.Version (showVersion)
import Graphics.Svg (loadSvgFile, saveXmlFile)
import qualified Options.Applicative as O
import Paths_svg_simplify (version)
import Simplify (simplifyDocument)
import System.Directory (copyFile)
import System.Exit (exitFailure, exitSuccess)

newtype Options
  = Options
      { _filename :: FilePath
      }

data CmdArgs = CmdVersion | CmdOptions Options

parser :: O.ParserInfo CmdArgs
parser =
  O.info
    (O.helper <*> (ver <|> (optsParser <&> CmdOptions)))
    ( O.fullDesc
        <> O.progDesc "Modify SVG files to use more basic features but look the same"
        <> O.header "svg-simplify - improve SVG files compatibilty"
    )
  where
    ver =
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
        Just svg ->
          do
            copyFile filename (filename <> ".bak")
            saveXmlFile filename (simplifyDocument svg)
