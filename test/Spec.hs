{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


import           Data.List              (intercalate)
import           Marvin.Interpolate.All
import           Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Lazy as L


data G = G

instance Show G where
    show _ = "show"

instance ShowStr G where
    showStr _ = "showStr"

instance ShowT G where
    showT _ = "showT"

instance ShowL G where
    showL _ = "showL"


main :: IO ()
main = hspec $ do
    describe "parsing to itself" $ do
        it "#" $
            [iq|#|] `shouldBe` "#"
        it "#anything" $
            [iq|#anything|] `shouldBe` "#anything"

    describe "parsing escape sequences" $
        it "parses ## as #" $
            [iq|##|] `shouldBe` "#"

    describe "interpolation substitution" $ do
        it "leaves an empty string" $
            [iq||] `shouldBe` ""
        it "returns a string unchanged if no variable is in it" $
            [iq|this is not changed|] `shouldBe` "this is not changed"
        it "interpolates just an external variable" $
            let y = "str" in
                [iq|#{y}|] `shouldBe` y
        it "interpolates an external variable" $
            let y = "str2" in [iq| hello you #{y} end|] `shouldBe` " hello you " ++ y ++ " end"
        it "interpolates the variable x (used to be special)" $
            let x = "str" in [iq| hello x: #{x}|] `shouldBe` " hello x: " ++ x
        it "interpolates infix with $" $
            [iq|str #{show $ 4 + (5 :: Int)} str|] `shouldBe` "str 9 str"
        it "interpolates multiple bindings" $
            let
                x = "multiple"
                y = "can"
                z = "local scope"
            in [iq|We #{y} interpolate #{x} bindings from #{z}|]
                `shouldBe` "We can interpolate multiple bindings from local scope"

        it "interpolates complex expressions" $
            let
                x = ["haskell", "expression"]
                y = " can be"
            in [iq|Any #{intercalate " " x ++ y} interpolated|]
                 `shouldBe` "Any haskell expression can be interpolated"


    describe "splice interpolation" $
        it "interpolates a splice" $
            let x = 5 :: Int in $(isS "#{x}") `shouldBe` "5"
    
    describe "'is' generic interpolation" $ do
        it "to string" $
            $(is "") `shouldBe` ("" :: String)
        it "to Text" $
            $(is "") `shouldBe` ("" :: T.Text)
        it "to lazy Text" $ 
            $(is "") `shouldBe` ("" :: L.Text)
    
    let x = 5 :: Int

    describe "'isS' interpolation to String" $ do
        it "calls show on Int" $
            $(isS "#{x}") `shouldBe` "5"
        it "calls showStr if available" $
            $(isS "#{G}") `shouldBe` "showStr"
        it "does not change Text" $
            $(isS "#{\"str\" :: T.Text}") `shouldBe` "str"
        it "does not change String" $
            $(isS "#{\"str\" :: String}") `shouldBe` "str"

    describe "'isT' interpolation to Text" $ do
        it "calls show on Int" $
            $(isT "#{x}") `shouldBe` "5"
        it "calls showT if available" $
            $(isT "#{G}") `shouldBe` "showT"
        it "does not change Text" $
            $(isT "#{\"str\" :: T.Text}") `shouldBe` "str"
        it "does not change String" $
            $(isT "#{\"str\" :: String}") `shouldBe` "str"

    describe "'isL' interpolation to lazy Text" $ do
        it "calls show on Int" $
            $(isL "#{x}") `shouldBe` "5"
        it "calls showStr if available" $
            $(isL "#{G}") `shouldBe` "showL"
        it "does not change Text" $
            $(isL "#{\"str\" :: T.Text}") `shouldBe` "str"
        it "does not change String" $
            $(isL "#{\"str\" :: String}") `shouldBe` "str"
