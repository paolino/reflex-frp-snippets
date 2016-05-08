{-# LANGUAGE RecursiveDo, QuasiQuotes,TemplateHaskell,ScopedTypeVariables, FlexibleContexts,NoMonomorphismRestriction, ConstraintKinds#-}
import Reflex.Dom
import Data.FileEmbed (embedStringFile)
import Data.String.Here (here)
import Layout (mainWidgetWithAssets)
import Data.Map (fromList)

type DS = Dynamic Spider
type ES = Event Spider
type MS = MonadWidget Spider

data Color = White | Red deriving Show

light :: MS m => DS Color ->  m ()
light color = do
    attrs <- mapDyn (\c -> fromList [("style","background-color:" ++ show c),("class","light")]) color
    elDynAttr "div" attrs $ return ()

wiring :: MS m => m ()
wiring = do
    btn <- fmap fst . el' "button" $ text "light"
    let es = [Red <$ domEvent Mousedown btn, White <$ domEvent Mouseup btn]
    color <- holdDyn White $ leftmost es
    light color

-- putting the button after the light, but "btn" binding is needed before.
-- We use MonadFix and rec syntax to resolve it.
-- Care must be taken with "rec". Try to remove signature to es
wiringReorder :: MS m => m ()
wiringReorder = do 
    rec     let (es :: ES Color) = leftmost [Red <$ domEvent Mousedown btn, White <$ domEvent Mouseup btn]
            color <- holdDyn White es 
            light color                                                                    
            btn <- fmap fst . el' "button" $ text "light"                                  
    return ()
            

        
----------------------------- tutorial layout  ---------------------------------------

description = [here|
A button, controlling a two colors light
|]


main = mainWidgetWithAssets 
    "Simple button" 
    description  
    $(embedStringFile "AButton.hs") 
    (Just $(embedStringFile "AButton.css")) $ do
        wiring 
        wiringReorder


