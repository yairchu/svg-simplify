{-# LANGUAGE OverloadedStrings, TupleSections, DataKinds, TypeApplications #-}

module Simplify
  ( simplifyDocument,
  )
where

import Codec.Picture.Types (PixelRGBA8 (..))
import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrd)
import Data.Generics.Product
import Data.Generics.Sum
import Data.Hashable (Hashable (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Graphics.Svg
import Graphics.Svg.CssTypes
import Svg

simplifyDocument :: Document -> Document
simplifyDocument doc =
  doc
    & elements .~ fixedElems
    & definitions %~ Map.filter (not . Lens.has (_Ctor @"ElementMask"))
    & definitions <>~ newDefs
    & styleRules %~ squashCssClasses . fmap cssRuleRemoveMasks
  where
    (newDefs, fixedElems) = traverse (simplifyTree doc) (doc ^. elements)

cssRuleRemoveMasks :: CssRule -> CssRule
cssRuleRemoveMasks = field @"cssDeclarations" %~ filter ((/= "mask") . (^. field @"_cssDeclarationProperty"))

squashCssClasses :: [CssRule] -> [CssRule]
squashCssClasses =
    map (\(k, v) -> CssRule v k) .
    Map.toList . Map.fromListWith (<>) . map (\(k, v) -> (v, [k])) .
    Map.toList . Map.fromListWith (<>) . (>>= disperse)
    where
      disperse (CssRule sels decs) = sels <&> (, decs)

type Definitions = Map String Element

simplifyTree :: Document -> Tree -> (Definitions, Tree)
simplifyTree doc = simplifyTreeH doc mempty

type Context = (DrawAttributes, CssContext Tree)

simplifyTreeH :: Document -> Context -> Tree -> (Definitions, Tree)
simplifyTreeH doc parentCtx tree =
  do
    fixedTree <- treeChildren (simplifyTreeH doc ctx) tree
    case fixedTree of
      GroupTree {} -> pure fixedTree
      SymbolTree {} -> pure fixedTree
      _ ->
        case drawAttrs ^? maskRef . traverse of
          Nothing -> (mempty, drawAttrs)
          Just maskId ->
            drawAttrs
              & maskRef .~ mempty
              & attrClass .~ []
              & attrId .~ Nothing
              & (fillColor . traverse) (fixTexture doc maskId)
              >>= (strokeColor . traverse) (fixTexture doc maskId)
          <&> \fixedAttrs ->
            GroupTree
              Group
                { _groupDrawAttributes = tree ^. drawAttr,
                  _groupViewBox = Nothing,
                  _groupAspectRatio = defaultPreserveAspectRatio,
                  _groupChildren =
                    [ fixedTree & drawAttr .~ fixedAttrs
                    ]
                }
  where
    ctx@(drawAttrs, _) = accumulateContext (doc ^. styleRules) tree parentCtx

accumulateContext :: [CssRule] -> Tree -> Context -> Context
accumulateContext cssRules tree (parentDrawAttrs, parentCtx) =
  ( parentDrawAttrs <> drawAttrsCss <> tree ^. drawAttr,
    ctx
  )
  where
    ctx = [tree] : parentCtx
    drawAttrsCss =
      mempty
        & maskRef .~ Last (maskRefFromCssDecls cssDecls)
        & fillColor .~ Last (textureFromCssDecls "fill" cssDecls)
        & strokeColor .~ Last (textureFromCssDecls "stroke" cssDecls)
    cssDecls =
      findMatchingDeclarations cssRules ctx
        <> do
          cls0 <- tree ^. drawAttr . attrClass
          CssRule selectors decls <- cssRules
          AllOf descs <- concat selectors
          OfClass cls1 <- descs
          cls0 == cls1 & guard
          decls

fixTexture :: Document -> ElementRef -> Texture -> (Definitions, Texture)
fixTexture _ RefNone x = pure x
fixTexture _ _ FillNone = pure FillNone
fixTexture doc (Ref maskId) x =
  case doc
    ^? definitions . Lens.ix maskId . _Ctor @"ElementMask" . maskContent . traverse
      . Lens.to (findFill (doc ^. styleRules) mempty)
      . Lens._Just of
    Nothing -> error "TODO: Mask not found?"
    Just FillNone -> pure FillNone
    Just ColorRef {} -> error "TODO: SVG uses color as mask?"
    Just (TextureRef maskTexture) ->
      case x of
        ColorRef col ->
          case maskDef of
            ElementLinearGradient grad ->
              grad
                & linearGradientStops . Lens.mapped %~ fixStop
                & ElementLinearGradient
                & makeTexture
            ElementRadialGradient grad ->
              grad
                & radialGradientStops . Lens.mapped %~ fixStop
                & ElementRadialGradient
                & makeTexture
            _ -> error "TODO: Unsupported mask fill"
          where
            fixStop stop
              | mr /= mg || mg /= mb = error "TODO: Colored mask?"
              | otherwise =
                stop
                  & gradientColor .~ col
                  & gradientOpacity ?~ (fromIntegral mr / 255 * (fromIntegral ma / 255))
              where
                PixelRGBA8 mr mg mb ma = stop ^. gradientColor
        TextureRef fillTexture ->
          case (fillDef, maskDef) of
            (ElementLinearGradient f, ElementLinearGradient m)
              | f ^. linearGradientStart == m ^. linearGradientStart
                  && f ^. linearGradientStop == m ^. linearGradientStop ->
                f
                  & linearGradientStops .~ (offsets <&> mkStop)
                  & ElementLinearGradient
                  & makeTexture
              where
                offsets =
                  f ^.. linearGradientStops . traverse . gradientOffset
                    <> m ^.. linearGradientStops . traverse . gradientOffset
                    & nubOrd
                mkStop offset
                  | mr /= mg || mg /= mb = error "TODO: Colored mask?"
                  | otherwise =
                    -- TODO: Don't just override prev opacity
                    stop
                      & gradientOpacity
                        ?~ (fromIntegral ma / 255 * (fromIntegral mr / 255))
                  where
                    stop = gradientLookup (f ^. linearGradientStops) offset
                    PixelRGBA8 mr mg mb ma =
                      gradientLookup (m ^. linearGradientStops) offset ^. gradientColor
            _ -> error "TODO: Unsupported fill/mask combo"
          where
            fillDef = doc ^?! definitions . Lens.ix fillTexture
      where
        maskDef = doc ^?! definitions . Lens.ix maskTexture

findFill :: [CssRule] -> Context -> Tree -> Maybe Texture
findFill cssRules parentCtx tree =
  drawAttrs ^. fillColor . Lens._Wrapped
    <|> (tree ^.. treeChildren <&> findFill cssRules ctx) ^? traverse . Lens._Just
  where
    ctx@(drawAttrs, _) = accumulateContext cssRules tree parentCtx

makeTexture :: Element -> (Definitions, Texture)
makeTexture x =
  (mempty & Lens.at key ?~ x, TextureRef key)
  where
    -- TODO: Better hash with no collisions
    key = "SVG_SIMPLIFY_" <> (show . abs . hash . show) x
