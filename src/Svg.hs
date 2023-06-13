{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Svg
  ( treeChildren,
    maskRefFromCssDecls,
    textureFromCssDecls,
    gradientLookup,
    defaultPreserveAspectRatio,
  )
where

import Codec.Picture.Types (PixelRGBA8 (..))
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Data.Generics.Product (HasField (..))
import Data.Generics.Sum (AsConstructor (..))
import Data.Text (Text, unpack)
import Graphics.Svg hiding (Text)
import Graphics.Svg.CssTypes

treeChildren :: Lens.Traversal' Tree Tree
treeChildren f (GroupTree x) = (groupChildren . traverse) f x <&> GroupTree
treeChildren f (SymbolTree x) = (groupOfSymbol . groupChildren . traverse) f x <&> SymbolTree
treeChildren _ x = pure x

maskRefFromCssDecls :: [CssDeclaration] -> Maybe ElementRef
maskRefFromCssDecls decls =
  decls
    ^? traverse
      . Lens.filteredBy (field @"_cssDeclarationProperty" . Lens.only "mask")
      . field @"_cssDeclarationValues"
      . traverse
      . traverse
      . cssRefFromElement
    <&> Ref . unpack

cssRefFromElement :: Lens.Traversal' CssElement Text
cssRefFromElement =
  _Ctor @"CssFunction"
    . Lens.filteredBy (Lens._1 . Lens.only "url")
    . Lens._2
    . traverse
    . _Ctor @"CssReference"

textureFromCssDecls :: Text -> [CssDeclaration] -> Maybe Texture
textureFromCssDecls attribute decls =
  decls
    ^? traverse
      . Lens.filteredBy (field @"_cssDeclarationProperty" . Lens.only attribute)
      . field @"_cssDeclarationValues"
      . traverse
      . traverse
      . Lens.failing
        (_Ctor @"CssColor" . Lens.to ColorRef)
        (cssRefFromElement . Lens.to (TextureRef . unpack))

gradientLookup :: [GradientStop] -> Float -> GradientStop
gradientLookup [] _ = error "Empty gradient!"
gradientLookup [x] _ = x
gradientLookup (x0 : x1 : xs) offset
  | offset == x0 ^. gradientOffset = x0
  | offset >= x1 ^. gradientOffset = gradientLookup (x1 : xs) offset
  | otherwise =
      GradientStop
        { _gradientOffset = offset,
          _gradientColor = PixelRGBA8 (f r0 r1) (f g0 g1) (f b0 b1) (f a0 a1),
          _gradientOpacity = case (x0 ^. gradientOpacity, x1 ^. gradientOpacity) of
            (Nothing, Nothing) -> Nothing
            (Just o0, Nothing) -> fFloat o0 1 & Just
            (Nothing, Just o1) -> fFloat 1 o1 & Just
            (Just o0, Just o1) -> fFloat o0 o1 & Just,
          _gradientPath = Nothing -- TODO: Not yet supported
        }
  where
    PixelRGBA8 r0 g0 b0 a0 = x0 ^. gradientColor
    PixelRGBA8 r1 g1 b1 a1 = x1 ^. gradientColor
    f c0 c1 = fFloat (fromIntegral c0) (fromIntegral c1) & round
    fFloat c0 c1 = c0 + (c1 - c0) * mix
    mix = (offset - x0 ^. gradientOffset) / (x1 ^. gradientOffset - x0 ^. gradientOffset)

defaultPreserveAspectRatio :: PreserveAspectRatio
defaultPreserveAspectRatio =
  PreserveAspectRatio
    { _aspectRatioDefer = False,
      _aspectRatioAlign = AlignxMidYMid,
      _aspectRatioMeetSlice = Nothing
    }
