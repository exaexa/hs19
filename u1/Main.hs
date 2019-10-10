module Main where

import Brick
import qualified Brick.Types as T
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as Color

{- 
 - ScreenState is our datatype for storing the state of the screen. The record
 - syntax is slightly more similar to the C-style structures; the "filed names"
 - are in fact accessor functions with types as such:
 - curPos :: ScreenState -> Int
 -}
data ScreenState = ScreenState
  { curPos :: Int
  , curHighlighted :: Bool
  }

{- this is for creating the initial state -}
initState = ScreenState {curPos = 0, curHighlighted = False}

{- This function converts the state to a list of widget layers to be drawn,
 - we only draw a single layer. -}
renderApp s = [center (title <=> mainLayer s)]

{- Operator <=> combines widgets vertically (as seen above),
 - operator <+> combines them horizontally. -}
mainLayer s = foldr1 (<+>) $ map (letter s) [0 .. 10]

title = border $ str "Demo app!"

cursorHl = attrName "cursorHl"

letter s i
  | curPos s == i =
    (if curHighlighted s
       then withAttr cursorHl
       else id) $
    str "X"
  | otherwise = str "-"

{- 
 - Event handlers that modify the state. They should in fact return a monad,
 - but that is vastly simplified by the helper functions:
 -
 - halt newstate   -- halts the app
 - continue newstate    -- continues executing the app with the supplied state
 -
 - The record update syntax is used as such:
 - originalItem{modifiedField = new value}
 -}
handleEvent s (T.VtyEvent (V.EvKey k [])) =
  case k of
    V.KEsc -> halt s
    V.KLeft -> continue $ s {curPos = max 0 (curPos s - 1)}
    V.KRight -> continue $ s {curPos = min 10 (curPos s + 1)}
    V.KChar ' ' -> continue $ s {curHighlighted = not (curHighlighted s)}
    _ -> continue s --ignore all other keys
handleEvent s _ = continue s --also ignore all other events

{- This constructs the Brick app from the available functions -}
app :: App ScreenState e ()
app =
  App
    { appDraw = renderApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap =
        const $ attrMap V.defAttr [(cursorHl, Color.black `on` Color.cyan)]
    }

{- main just starts Brick main and discards (using void) the final state -}
main :: IO ()
main = void $ defaultMain app initState
