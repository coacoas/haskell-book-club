# Intermission

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

applyTimes 5 (+1) 5
(+1) (applyTimes (4) (+1) 5)
(+1) ((+1) applyTimes (3) (+1) 5)
(+1) ((+1) ((+1) applyTimes (2) (+1) 5))
(+1) ((+1) ((+1) ((+1) applyTimes (1) (+1) 5)))
(+1) ((+1) ((+1) ((+1) ((+1) applyTimes (0) (+1) 5))))
(+1) ((+1) ((+1) ((+1) ((+1) 5))))
(+1) ((+1) ((+1) ((+1) 6)))
(+1) ((+1) ((+1) 7))
(+1) ((+1) 8)
(+1) 9
10
```


# Chapter Exercises


## Review Of Types

\`1\`. What is the type of `[[True, False], [True, True], [False, True]]`?

(d) `[[Bool]]`

---

\`2\`. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?

(b) [[3 == 3], [6 > 5], [3 < 4]]

---

\`3\`. For the following function

```haskell
func    :: [a] -> [a] -> [a]
func x y = x ++ y
```

which of the following is true?

(d) all of the above

---

\`4\`. For the `func` code above, which is a valid application of `func` to both of its arguments?

(b) `func "Hello" "World"`


## Review currying

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

---

What is the value of `appedCatty "woohoo"`?

```haskell
"woops mrow woohoo"
```

---

`frappe "1"`?

```haskell
"1 mrow haha"
```

---

`frappe (appedCatty "2")`?

```haskell
"woops mrow 2 mrow haha""
```

---

`appedCatty (frappe "blue")`?

```haskell
"woops mrow blue mrow haha"
```

---

```haskell
cattyConny (frappe "pink")
           (cattyConny "green" (appedCatty "blue"))
```

```haskell
"ping mrow haha mrow green mrow woops mrow blue"
```

---

```haskell
cattyConny (flippy "Pugs" "are") "awesome"
```

```haskell
"are mrow Pugs mrow awesome"
```


## Recursion

---

-   Write out the steps for reducing `dividedBy 15 2`

```haskell
dividedBy 15 2
go 15 2 0
go 13 2 1 -- (15 - 2) 2 (0 + 1)
go 11 2 2 -- (13 - 2) 2 (1 + 1)
go 9  2 3 -- (11 - 2) 2 (2 + 1)
go 7  2 4 -- (9  - 2) 2 (3 + 1)
go 5  2 5 -- (7  - 2) 2 (4 + 1)
go 3  2 6 -- (5  - 2) 2 (5 + 1)
go 1  2 7 -- (3  - 2) 2 (6 + 1)
-- 1 < 2, so return (7, 1)
```

---

-   Write a function that recursively sums all numbers from 1 to n.

```haskell
sum :: (Num a, Eq a) => a -> a
sum n = go n 0
  where go x acc
         | x == 0    = acc
         | otherwise = go (x - 1) (acc + x)
```

```haskell
sum :: (Num a, Ord a) => a -> a
sum n = go n 0
  where go x acc
         | x <= 0    = acc
         | otherwise = go (x - 1) (acc + x)
```

---

-   Write a function that multiplies two integral numbers using recursive summation.

```haskell
mult :: (Integral a) => a -> a -> a
mult x y = go x 0
  where go n acc
         | n == 0     = acc
         | n > 0      = go (n - 1) (acc + y)
         | otherwise  = go (n + 1) (acc - y)
```


## Fixing `dividedBy`

Fix `dividedBy` to work properly with div-by-zero and negative values

```haskell
-- Coming back to this one... 
```


## McCarthy 91

```haskell
mc :: Integral a => a -> a
mc n
 | n > 100 = n - 10
 | otherwise = mc . mc $ n + 11
```


## Numbers Into Words

```haskell
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "how did you get this number?"
```

```
digits :: Int -> [Int]
digits n = go n []
  where go x acc
          | x < 10 = x : acc
          | otherwise = let (next, digit) = divMod x 10
                        in go next (digit : acc)

wordNumber :: Int -> String
wordNumber n =
  let ds = digits n
      words = map digitToWord ds
      grouped = intersperse "-" words
  in concat grouped

```
