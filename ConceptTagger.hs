{-# LANGUAGE RecursiveDo, GADTs, QuasiQuotes,TemplateHaskell, TupleSections, ScopedTypeVariables, ConstraintKinds, FlexibleContexts#-}
import Reflex.Dom
import Control.Monad (void,forM,forM_)
import Data.FileEmbed (embedStringFile)
import Data.String.Here (here,template,i)
import Layout (mainWidgetWithAssets)
import Lib (ES,MS,DS,BS,Plug, pick, mergeDSums,domMorph)
import Control.Lens (view,(.~),(&))
-- import Data.GADT.Compare
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.Dependent.Map (DMap,DSum( (:=>) ))

-- what we want to create and collect
type Tagged = (String,Int)

-- the 2 possible events coming from the tagger widget. 
-- By using gadts we prepare the right types for primitive
-- *merge*, here used by mergeDSums
data TaggerPlug a where
    Length :: TaggerPlug Int
    Word :: TaggerPlug Tagged

-- necessary instances for TaggerPlug
deriveGEq ''TaggerPlug
deriveGCompare ''TaggerPlug

-- the variable number tagger input fields collection 
-- this will be re-evaluated when argument changes
tagger  :: MS m   
        => Int  -- number of tags/input
        -> m (Plug TaggerPlug) -- events of *type* TaggerPlug 
tagger ntags = do
    m <- ffilter (<=10) <$> (ntags + 1 <$) <$> button "more tags"
    l <- ffilter (>= 1) <$> (ntags - 1 <$) <$> button "less tags"
    -- collect the production of Tagged, a different signal for each tag value
    ts :: [ES Tagged] <- 
        el "ol" . forM [1..ntags] $ \tag ->  do 
            el "li" $ do 
                rec     let -- changing a default via lens, setting the update
                            -- signal with an empty string coming from the "Enter" signal
                            resettable = def & setValue .~ ("" <$) enter 
                            -- an observable signal from the input field content
                            value :: BS String = current . view textInput_value $ input
                            -- couple the value of the k to each signal of "Enter" from 
                            -- the input field, weared by the i value
                            enter :: ES Tagged = attach value <$> (tag <$) $ textInputGetEnter input
                        input <- textInput resettable
                -- return the ES Tagged relative to this tag 
                return enter
    -- sum signals up to TaggerPlug and then merge out
    -- leftmost is forgetful in case of synchronicity, mergeDSums not.
    return $ mergeDSums [Length :=> leftmost [m,l],Word :=> leftmost ts] 

-- the result collection of tagged just shown in correct order
history :: MS m => [Tagged] -> m (ES ())
history ws = do 
    el "ul" . forM (reverse ws) $ \(w,i) -> 
        elClass "li" "record" $ do 
            el "span" $ text (show i)
            el "span" $ text w
    return never

wiring = do
    rec     -- track the changing number of tags from tagger(more + less)
            ntags :: DS Int <- holdDyn 10 (pick Length inputs :: ES Int)
            -- the plug from the inputs widget
            inputs :: Plug TaggerPlug <- el "span" $ domMorph tagger ntags
            -- collect the taggeds produced by tagger
            taggeds <- foldDyn (:) [] (pick Word inputs :: ES Tagged)
            -- 
            el "span" $ domMorph history taggeds
        
    return ()
        
----------------------------- tutorial layout  ---------------------------------------

description = [here|
Create a label tagged text list, with variable number of tags. 
labels are 2 state, an input field to define them or a button with
the name on it to redefine them
|]


main = mainWidgetWithAssets 
    "Dynamic List" 
    description  
    $(embedStringFile "ConceptTagger.hs") 
    (Just $(embedStringFile "NumberTagger.css")) 
    wiring


