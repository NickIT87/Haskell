module Main where

-- Predicate a is simply a function that takes a value of type a
-- and returns a Bool. In logic, a predicate represents a statement about an object.
type Predicate a = a -> Bool

-- Predicate Human(x)
-- Returns True if the string is in the list of known humans
human :: String -> Bool
human x = x `elem` ["Socrates", "Plato"]

-- Predicate God(x)
-- Returns True if the element is a god
god :: String -> Bool
god x = x == "Zeus"

-- Predicate Mortal(x)
-- In our model, every human is mortal.
-- So mortality is exactly the same as being human.
mortal :: String -> Bool
mortal x = human x

-- Universal quantifier ∀ implemented as a function
-- Takes:
--   1) a list of elements (the domain)
--   2) a predicate
-- Returns True if the predicate is True for ALL elements in the list
forAll :: [a] -> Predicate a -> Bool
forAll xs p = all p xs

-- Logical implication P -> Q
-- In Boolean logic: P -> Q is equivalent to ¬P ∨ Q
implies :: Bool -> Bool -> Bool
implies p q = not p || q

-- The domain of discourse (list of objects to check)
domain :: [String]
domain = ["Socrates", "Plato", "Zeus"] -- Zeus is included as a non-human god

-- Formula:
-- ∀x. Human(x) -> Mortal(x)
law :: Bool
law = forAll domain (\x -> implies (human x) (mortal x))

-- Existential quantifier ∃
exists :: [a] -> Predicate a -> Bool
exists xs p = any p xs

-- Existential formula: ∃x. Mortal(x)
lawExists :: Bool
lawExists = exists domain mortal

-- Detailed per-element check for better output
checkEach :: IO ()
checkEach = mapM_ check domain
  where
    check x =
      putStrLn $
        "Checking: " ++ x ++ "\n" ++
        "  God(x)    = " ++ show (god x) ++ "\n" ++
        "  Human(x)  = " ++ show (human x) ++ "\n" ++
        "  Mortal(x) = " ++ show (mortal x) ++ "\n" ++
        "  Human(x) -> Mortal(x) = " ++ show (implies (human x) (mortal x)) ++ "\n"

main :: IO ()
main = do
  putStrLn "Checking the law: forall x. Human(x) -> Mortal(x)"
  putStrLn "------------------------------------------"
  checkEach
  putStrLn "Result:"
  putStrLn $ "Is the formula true for the entire domain? " ++ show law
  putStrLn ""
  putStrLn "Existential quantifier check: exists x. Mortal(x)"
  putStrLn "------------------------------------------"
  putStrLn $ "Is there at least one mortal? " ++ show lawExists