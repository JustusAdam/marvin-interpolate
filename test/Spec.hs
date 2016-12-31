{-# LANGUAGE QuasiQuotes #-}


import Test.Hspec
import Marvin.Interpolate


formatSpec :: Spec
formatSpec = 
    describe "marvin interpolation" $ do
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


main :: IO ()
main = hspec formatSpec
