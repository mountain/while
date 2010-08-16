-----------------------------------------------------------------------------
-- |
-- Module      :  Lang.While.Store
-- Copyright   :  {c} Mingli Yuan 2007
-- License     :  BSD-style {see the file libraries/base/LICENSE}
--
-- Maintainer  :  me@mingli-yuan.info
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Lang.While.Store where

import Prelude
import Data.Maybe
import qualified Data.Map as Map

import Lang.While.WhilyTree

type Bindings = Map.Map String WhilyTree

intializeStore :: Bindings
intializeStore = Map.empty

lookupVar :: String -> Bindings -> WhilyTree
lookupVar name bnds = fromJust (Map.lookup name bnds)

assign :: String -> WhilyTree -> Bindings -> Bindings
assign var val bnds = Map.insert var val bnds

