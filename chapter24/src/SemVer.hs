module SemVer where

import Control.Applicative
import Data.List (dropWhile, null)
import Text.Trifecta


  -- Relevant to precedence/ordering, -- cannot sort numbers like strings.

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Show, Eq)
type Major = Integer
type Minor = Integer
type Patch = Integer

type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  (NOSI <$> integer) <|> (NOSS <$> some letter)

parseDotSeparated :: Parser [NumberOrString]
parseDotSeparated = (sepBy parseNumberOrString (char '.'))

parseRelease :: Char -> Parser [NumberOrString]
parseRelease prefix =
  (char prefix *> parseDotSeparated) <|> (pure [])

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _     <- char '.'
  minor <- integer
  _     <- char '.'
  patch <- integer
  release <- parseRelease '-'
  metadata <- parseRelease '+'

  return $ SemVer major minor patch release metadata

-- instance Ord SemVer where
--   (SemVer maj1 min1 pat1 _ _) <= (SemVer maj2 min2 pat2 _ _) =
--     maj1 < maj2 ||
--     (maj1 == maj2 && min1 < min2) ||
--     (maj1 == maj2 && min1 == min2 && pat1 <= pat2)
   
instance Ord SemVer where
  (SemVer a1 b1 c1 _ _) <=  (SemVer a2 b2 c2 _ _) =
    let
      diffs = [a1 - a2, b1 - b2, c1 - c2]
      filtered = Data.List.dropWhile (==0) diffs
    in
      Data.List.null filtered || head filtered < 0
