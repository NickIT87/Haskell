import Graphics.Gloss

-- Display a red circle on a white background
main :: IO ()
main = display (InWindow "My Window" (400, 400) (0, 0)) white $ color red $ thickCircle 50 10
