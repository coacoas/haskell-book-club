module ChapterExercises where

ex1 :: [Bool]
ex1 = [
  True,
  False, -- Need (++)
  True,
  True,  -- Generates ["helloworld"], though.
  False, -- Should be hello !! 4
  True,
  False, -- Need parameter for take - it's included in the string
  True]

ex2 :: [(Char, Char)]
ex2 = [('a', 'd'), ('b', 'c'), ('c', 'e'), ('d', 'a'), ('e', 'b')]
