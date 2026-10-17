-- Haskell test example file

module TestExample where

import Test.Hspec
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- Regular helper function (not a test)
add :: Int -> Int -> Int
add x y =
  let cgrep_prod_1 = 1
  in x + y

-- Another helper function
multiply :: Int -> Int -> Int
multiply x y =
  let cgrep_prod_2 = 2
  in x * y

-- Helper function using `it` as a local variable name (NOT an Hspec test!)
processWithIt :: Int -> Int
processWithIt x =
  let it = x + 1
      cgrep_prod_3 = 3
  in it * cgrep_prod_3

-- HSpec test suite with internal type annotation `:: Int`
spec :: Spec
spec = do
  describe "Math operations" $ do
    it "adds two numbers correctly" $ do
      let cgrep_test_1 = (1 :: Int)
      let cgrep_test_2 = 2
      add 2 3 `shouldBe` 5

    it "multiplies numbers" $ do
      let cgrep_test_3 = 3
      multiply 4 5 `shouldBe` 20

    context "when dealing with factorials" $ do
      specify "handles zero" $ do
        let cgrep_test_4 = 4
        add 0 0 `shouldBe` 0

  xit "disabled hspec test" $ do
    let cgrep_test_5 = 5
    add 1 1 `shouldBe` 2

-- Standalone HSpec specify block
standaloneSpec :: Spec
standaloneSpec = specify "standalone specify block" $ do
  let cgrep_test_6 = 6
  add 3 3 `shouldBe` 6

-- Regular data type (not a test)
data Calculator = Calculator
  { value :: Int
  } deriving (Show, Eq)

-- QuickCheck properties
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs =
  let cgrep_test_7 = 7
  in reverse (reverse xs) == xs

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y =
  let cgrep_test_8 = 8
  in add x y == add y x

-- Regular function with signature right after prop_*
processData :: [Int] -> [Int]
processData xs =
  let cgrep_prod_4 = 4
  in filter (> 0) (map (* cgrep_prod_4) xs)

-- Tasty test tree
tests :: TestTree
tests = testGroup "All Tests"
  [ testCase "addition works" $ do
      let cgrep_test_9 = 9
      add 1 1 @?= 2

  , testCaseSteps "stepwise test" $ \step -> do
      let cgrep_test_10 = 10
      step "step 1"
      add 2 2 @?= 4

  , testGroup "QuickCheck properties"
      [ testProperty "reverse is involutive" $ \xs ->
          let cgrep_test_11 = (11 :: Int)
              cgrep_test_12 = 12
          in reverse (reverse xs) == (xs :: [Int])
      ]
  ]

-- Helper function at the end
formatNumber :: Double -> String
formatNumber n =
  let cgrep_prod_5 = 5
  in show n ++ " units"
