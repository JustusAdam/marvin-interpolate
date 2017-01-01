{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


import           Data.List              (intercalate)
import           Marvin.Interpolate.All
import           Test.Hspec


formatSpec :: Spec
formatSpec = do
    describe "parsing to itself" $ do
        it "%" $
            [iq|%|] `shouldBe` "%"
        it "%anything" $
            [iq|%anything|] `shouldBe` "%anything"
        it "~" $
            [iq|~|] `shouldBe` "~"
        it "~anything" $
            $(is "~anything") `shouldBe` "~anything"

    describe "parsing escape sequences" $ do
        it "parses ~% as %" $
            [iq|~%|] `shouldBe` "%"
        it "parses ~] as ]" $
            [iq|~]|] `shouldBe` "]"
        it "parses ~%{} as %{}" $
            [iq|~%{}|] `shouldBe` "%{}"
        it "parses |~] as |]" $
            [iq||~]|] `shouldBe` "|]"
        it "parses ~~ as ~" $
            [iq|~~|] `shouldBe` "~"

    describe "interpolation substitution" $ do
        it "leaves an empty string" $
            [iq||] `shouldBe` ""
        it "returns a string unchanged if no variable is in it" $
            [iq|this is not changed|] `shouldBe` "this is not changed"
        it "interpolates just an external variable" $
            let y = "str" in
                [iq|%{y}|] `shouldBe` y
        it "interpolates an external variable" $
            let y = "str2" in [iq| hello you %{y} end|] `shouldBe` " hello you " ++ y ++ " end"
        it "interpolates the variable x (used to be special)" $
            let x = "str" in [iq| hello x: %{x}|] `shouldBe` " hello x: " ++ x
        it "interpolates infix with $" $
            [iq|str %{show $ 4 + (5 :: Int)} str|] `shouldBe` "str 9 str"
        it "interpolates multiple bindings" $
            let
                x = "multiple"
                y = "can"
                z = "local scope"
            in [iq|We %{y} interpolate %{x} bindings from %{z}|]
                `shouldBe` "We can interpolate multiple bindings from local scope"

        it "interpolates complex expressions" $
            let
                x = ["haskell", "expression"]
                y = " can be"
            in [iq|Any %{intercalate " " x ++ y} interpolated|]
                 `shouldBe` "Any haskell expression can be interpolated"


    describe "splice interpolation" $
        it "interpolates a splice" $
            let x = 5 in $(isS "%{x}") `shouldBe` "5"


main :: IO ()
main = hspec formatSpec
