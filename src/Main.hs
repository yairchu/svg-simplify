{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens.Operators
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Version (showVersion)
import Graphics.Svg (loadSvgFile, xmlOfDocument)
import qualified Options.Applicative as O
import Paths_svg_simplify (version)
import Simplify (simplifyDocument)
import System.Directory (copyFile)
import System.Exit (exitFailure, exitSuccess)
import qualified Text.XML.Light.Output as XML

newtype Options
  = Options
      { _filenames :: [FilePath]
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
        <$> O.some (O.argument O.str (O.metavar "FILES..."))

getOpts :: IO Options
getOpts =
  O.execParser parser
    >>= \case
      CmdVersion -> putStrLn ("svg-simplify version " ++ showVersion version) >> exitSuccess
      CmdOptions o -> return o

processFile :: FilePath -> IO ()
processFile filename =
  loadSvgFile filename
    >>= \case
      Nothing -> putStrLn ("Failed to parse SVG file: " <> filename) >> exitFailure
      Just svg ->
        result
          `seq` do
            copyFile filename (filename <> ".bak")
            B.writeFile filename result
        where
          result =
            simplifyDocument svg
              & xmlOfDocument
              & XML.ppcTopElement XML.prettyConfigPP
              & T.pack
              & T.encodeUtf8

main :: IO ()
main =
  do
    Options filenames <- getOpts
    traverse_ processFile filenames
