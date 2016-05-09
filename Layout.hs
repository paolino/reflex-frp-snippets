{-# LANGUAGE RecursiveDo, QuasiQuotes,TemplateHaskell, ScopedTypeVariables, ConstraintKinds, FlexibleContexts#-}
module Layout where

import qualified Data.Map as M
import Reflex.Dom
import Control.Monad (void)
import Reflex.Contrib.Utils (end)
import Language.Haskell.HsColour.Colourise
import Language.Haskell.HsColour
import Control.Monad
import Data.FileEmbed
import Data.String.Here
import GHCJS.DOM.Document (setTitle)
import Lib

this = $(embedStringFile "Layout.hs")

css = $(embedStringFile "Tutorial.css")

sep :: MS m => m ()
sep = divClass "header" $ end

showCode hs = do
    holdDyn (color hs) never >>= 
            elDynHtmlAttr' "div" (M.singleton "class" "code")
    where
        gray = Foreground (Rgb 120 120 120)
        color = hscolour ICSS  defaultColourPrefs{varop=[gray],layout=[gray]} False True "Source" False

mainWidgetWithAssets title description hs mcss w = mainWidget . void $ do
    
    askDocument >>= flip setTitle (Just title)
    el "style" $ text $ css
    maybe end (el "style" . text) mcss
    divClass "header" $ text $ description 
    divClass "app" w
    
    mapM_ (\x -> sep >> showCode x) $ hs ++ [this]
    sep
    divClass "css" $ text css
