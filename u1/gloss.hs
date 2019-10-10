import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

initialWorld = 5

drawWorld n =
  Color black $
  Pictures $
  flip map [1 .. n] $ \i ->
    Translate (100 * fromInteger i - 550) 0 $ ThickCircle 50 20

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) n = max 0 $ n - 1
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) n = min 10 $ n + 1
handleEvent _ n = n

updateWorld _ = id

main = play FullScreen white 25 initialWorld drawWorld handleEvent updateWorld
