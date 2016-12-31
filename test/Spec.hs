{-# LANGUAGE QuasiQuotes #-}


import           Data.List              (intercalate)
import           Marvin.Interpolate.All
import           Test.Hspec


formatSpec :: Spec
formatSpec = do
    describe "escape sequences" $ do
        it "parses \\\\ as \\" $
            [i|\\|] `shouldBe` "\\"
        it "parses \\% as %" $
            [i|\%|] `shouldBe` "%"
        it "parses \\] as ]" $
            [i|\]|] `shouldBe` "]"
        it "parses \\%{} as %{}" $
            [i|\%{}|] `shouldBe` "%{}"
        it "parses |\\] as |]" $
            [i||\]|] `shouldBe` "|]"

    describe "interpolation substitution" $ do
        it "leaves an empty string" $
            [i||] `shouldBe` ""
        it "returns a string unchanged if no variable is in it" $
            [i|this is not changed|] `shouldBe` "this is not changed"
        it "interpolates just an external variable" $
            let y = "str" in
                [i|%{y}|] `shouldBe` y
        it "interpolates an external variable" $
            let y = "str2" in [i| hello you %{y} end|] `shouldBe` " hello you " ++ y ++ " end"
        it "interpolates the variable x (used to be special)" $
            let x = "str" in [i| hello x: %{x}|] `shouldBe` " hello x: " ++ x
        it "interpolates infix with $" $
            [i|str %{show $ 4 + (5 :: Int)} str|] `shouldBe` "str 9 str"
        it "interpolates multiple bindings" $
            let
                x = "multiple"
                y = "can"
                z = "local scope"
            in [i|We %{y} interpolate %{x} bindings from %{z}|]
                `shouldBe` "We can interpolate multiple bindings from local scope"

        it "interpolates complex expressions" $
            let
                x = ["haskell", "expression"]
                y = " can be"
            in [i|Any %{intercalate " " x ++ y} interpolated|]
                 `shouldBe` "Any haskell expression can be interpolated"

main :: IO ()
main = hspec formatSpec
