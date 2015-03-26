module Dicom.Gen.DictionarySpec (spec) where

import Dicom.Gen.Dictionary

import Test.Hspec

spec :: Spec
spec =
    describe "main" $ do
        it "returns the unit" $
            main `shouldReturn` ()
