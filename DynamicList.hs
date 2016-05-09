{-# LANGUAGE RecursiveDo, QuasiQuotes,TemplateHaskell, ScopedTypeVariables, ConstraintKinds, FlexibleContexts#-}
import qualified Data.Map as M
import Reflex.Dom
import Reflex.Contrib.Utils (end)
import Control.Monad -- (void,forM)
import Data.FileEmbed
import Data.String.Here
import Layout (mainWidgetWithAssets)
import Lib (mapMorph,ES,MS)


g :: MS m => Int -> m (ES Int)
g n = do
    m <- ffilter (<=10) <$> (n + 1 <$) <$> button "more"
    l <- ffilter (>= 1) <$> (n - 1 <$) <$> button "less"
    el "ul" . forM [1..n] $ \i -> 
         el "li" $ text (show i ++ "-")
    return $ leftmost [m,l]

wiring = do
    rec     n <- holdDyn 1 t
            t <- mapMorph dyn g n        
    return ()
        
----------------------------- tutorial layout  ---------------------------------------

description = [here|
Count 1 to 10 with li elements.
|]

main = mainWidgetWithAssets "Dynamic List" description  $(embedStringFile "DynamicList.hs") Nothing $ wiring

