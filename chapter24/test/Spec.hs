{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Lib
import Control.Applicative
import qualified Data.Map as M
import Test.Hspec
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta

import SemVer
import Data.Ini
import IntParser
import PhoneNumbers

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

pnParse = maybeSuccess . parseString parsePhone mempty

main :: IO ()
main = hspec $ do

  describe "parseNumberOrString" $ do
    it "can parse a number to NOSI" $ do
      let m = parseString parseNumberOrString mempty "1234"
          r' = maybeSuccess m
--      print m
      r' `shouldBe` Just (NOSI 1234)

    it "can parse a string to NOSS" $ do
      let m = parseString parseNumberOrString mempty "test"
          r' = maybeSuccess m
--      print m
      r' `shouldBe` Just (NOSS "test")

  describe "Semantic Version Parsing" $ do
    it "can parse a basic semantic version" $ do
      let m = parseString parseSemVer mempty "2.1.1"
          r' = maybeSuccess m
--      print m
      r' `shouldBe` Just (SemVer 2 1 1 [] [])

    it "can parse the release information" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.7.z.92"
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

  describe "Semantic Version Ordering" $ do
    it "can compare two versions according to the site" $ do
      (SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []) `shouldBe` True
    it "can compare two versions the other way" $ do
      (SemVer 2 1 1 [] [] < SemVer 2 1 0 [] []) `shouldBe` False
    it "can compare equality" $ do
      (SemVer 2 1 1 [] [] == SemVer 2 1 1 [] []) `shouldBe` True

  describe "parseDigit" $ do
    it "can parse a digit" $ do
      let m = parseString parseDigit mempty "0"
          r' = maybeSuccess m
      r' `shouldBe` Just '0'

    it "should not parse non-digits" $ do
      let m = parseString parseDigit mempty "a"
          r' = maybeSuccess m
      r' `shouldBe` Nothing

  describe "base10Integer" $ do
    it "can parse multiple digits into an Integer" $ do
      let m = parseString base10Integer mempty "12345"
          r' = maybeSuccess m
      r' `shouldBe` Just 12345

  describe "base10Integer" $ do
    it "can parse multiple digits into an Integer" $ do
      let m = parseString base10Integer' mempty "12345abc"
          r' = maybeSuccess m
      r' `shouldBe` Just 12345

    it "can parse a negative Integer" $ do
      let m = parseString base10Integer' mempty "-12345abc"
          r' = maybeSuccess m
      r' `shouldBe` Just (-12345)

  describe "Phone number parser" $ do
    it "can parse with dashes" $ do
      pnParse "123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse without dashes" $ do
      pnParse "1234567890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse with parentheses and spaces" $ do
      pnParse "(123) 456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse with leading 1" $ do
      pnParse "1-123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)

     -- it "can parse the metadata information" $ do
     --   let m = parseString parseSemVer mempty "1.0.0-+x.7.z.92"
     --       r' = maybeSuccess m
     --   print m
     --   r' `shouldBe` Just (SemVer 1 0 0 [] [NOSS "x", NOSI 7, NOSS "z", NOSI 92])
