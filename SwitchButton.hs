{-# LANGUAGE RecursiveDo, QuasiQuotes,TemplateHaskell,ScopedTypeVariables, FlexibleContexts,NoMonomorphismRestriction, ConstraintKinds#-}
import Reflex.Dom
import Data.FileEmbed (embedStringFile)
import Data.String.Here (here)
import Layout (mainWidgetWithAssets)
import Data.Map (fromList)

type DS = Dynamic Spider
type ES = Event Spider
type MS = MonadWidget Spider
type BS = Behavior Spider

data Color = White | Red deriving Show

light :: MS m => DS Color ->  m ()
light color = do
    attrs <- mapDyn (\c -> fromList [("style","background-color:" ++ show c),("class","light")]) color
    elDynAttr "div" attrs $ return ()

driver :: MS m => ES Color -> ES Color -> m (DS Bool)
driver e1 e2 = do
    flipper <- (not <$) <$> button "switch"
    choice <- foldDyn ($) True flipper
    let e = switch $ (\t -> if t then e1 else e2) <$> (current choice)
    holdDyn White e >>= light
    return choice

lighter :: MS m => String -> DS Bool -> m (ES Color)
lighter name attr = do
    bg <- mapDyn ((\c -> fromList [("style","background-color:" ++ c),("type","button")]).
                    (\t -> if t then "yellow" else "grey")) attr
    btn <- fmap fst . elDynAttr' "button" bg $ text name
    return $ leftmost 
        [Red <$ domEvent Mousedown btn, White <$ domEvent Mouseup btn]

    
wiring :: MS m => m ()
wiring = do
    rec     c <- driver l1 l2
            c' <- mapDyn not c
            l1 <- lighter "light-1" c
            l2 <- lighter "light-2" c'
    return ()

            

        
----------------------------- tutorial layout  ---------------------------------------

description = [here|
Two buttons controlling the same light, 
one wired at a time, with the wired in evidence.
A button to switch between the two.
|]


main = mainWidgetWithAssets 
    "Switch button" 
    description  
    $(embedStringFile "SwitchButton.hs") 
    (Just $(embedStringFile "SwitchButton.css")) $ do
        wiring 


