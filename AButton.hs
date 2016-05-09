{-# LANGUAGE RecursiveDo, QuasiQuotes,TemplateHaskell,ScopedTypeVariables, FlexibleContexts,NoMonomorphismRestriction, ConstraintKinds#-}
import Reflex.Dom
import Data.FileEmbed (embedStringFile)
import Data.String.Here (here)
import Layout (mainWidgetWithAssets)
import Data.Map (fromList)
import GHCJS.Foreign.QQ
import Control.Monad.Trans

type DS = Dynamic Spider
type ES = Event Spider
type MS = MonadWidget Spider

data Color = White | Red deriving Show

touchMouse x y w = leftmost 
        [   x <$ domEvent Mousedown w
        ,   y <$ domEvent Mouseup w
        ,   x <$ domEvent Touchstart w
        ,   y <$ domEvent Touchend w]

light :: MS m => DS Color ->  m ()
light color = do
    attrs <- mapDyn (\c -> fromList [("style","background-color:" ++ show c),("class","light")]) color
    elDynAttr "div" attrs $ return ()

wiring :: MS m => m ()
wiring = do
    btn <- fmap fst . el' "button" $ text "light"
    color <- holdDyn White $ touchMouse Red White btn
    light color

----------------------------- tutorial layout  ---------------------------------------

description = [here|
A button, controlling a two colors light
|]


main = mainWidgetWithAssets 
    "Simple button" 
    description  
    [$(embedStringFile "AButton.hs")]
    (Just $(embedStringFile "AButton.css")) $ do
        wiring 


