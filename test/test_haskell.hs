-- Haskell test example file

module TestExample where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- Regular helper function (not a test)
add :: Int -> Int -> Int
add x y = x + y

-- Another helper function
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Helper function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HSpec test suite
spec :: Spec
spec = do
  describe "Math operations" $ do
    it "adds two numbers correctly" $ do
      add 2 3 `shouldBe` 5
      add 10 20 `shouldBe` 30
    
    it "multiplies numbers" $ do
      multiply 4 5 `shouldBe` 20
    
    context "when dealing with factorials" $ do
      it "calculates factorial of 5" $ do
        factorial 5 `shouldBe` 120
      
      it "handles zero" $ do
        factorial 0 `shouldBe` 1

-- Regular data type (not a test)
data Calculator = Calculator
  { value :: Int
  } deriving (Show, Eq)

-- Helper functions for Calculator
makeCalculator :: Int -> Calculator
makeCalculator n = Calculator { value = n }

addToCalc :: Int -> Calculator -> Calculator
addToCalc n calc = calc { value = value calc + n }

-- QuickCheck properties
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = add x y == add y x

prop_multiplyAssociative :: Int -> Int -> Int -> Bool
prop_multiplyAssociative x y z = multiply x (multiply y z) == multiply (multiply x y) z

-- Regular function
processData :: [Int] -> [Int]
processData = filter (> 0) . map (* 2)

-- Tasty test tree
tests :: TestTree
tests = testGroup "All Tests"
  [ testCase "addition works" $ do
      add 1 1 @?= 2
      add 5 7 @?= 12
  
  , testCase "Calculator initialization" $ do
      let calc = makeCalculator 10
      value calc @?= 10
  
  , testGroup "QuickCheck properties"
      [ testProperty "reverse is involutive" prop_reverseReverse
      , testProperty "addition is commutative" prop_addCommutative
      ]
  
  , testGroup "Nested group"
      [ testCase "nested test 1" $
          multiply 2 3 @?= 6
      , testCase "nested test 2" $
          factorial 3 @?= 6
      ]
  ]

-- Helper function at the end
formatNumber :: Double -> String
formatNumber n = show n ++ " units"

-- More regular code
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

makePerson :: String -> Int -> Person
makePerson n a = Person { name = n, age = a }

-- Main function (not a test, but test runner)
main :: IO ()
main = do
  hspec spec
  defaultMain tests