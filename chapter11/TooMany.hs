{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
tooMany :: a -> Bool

instance TooMany (Int, String) where
tooMany (i, _) = i > 42

instance TooMany (Int, Int) where
tooMany (i, j) = i + j > 42

instance TooMany (Num a, Int b) where
tooMany (i, j) = i > 42

instance TooMany Int where
tooMany n = n > 42

