module Simplify where

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Graphics.Svg

simplifyDocument :: Document -> Document
simplifyDocument =
  elements . Lens.mapped %~ simplifyTree

simplifyTree :: Tree -> Tree
simplifyTree (GroupTree g) = g & groupChildren . Lens.mapped %~ simplifyTree & GroupTree
simplifyTree x = x
