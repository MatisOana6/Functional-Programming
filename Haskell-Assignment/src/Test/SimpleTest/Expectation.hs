{-# LANGUAGE GADTs #-}

module Test.SimpleTest.Expectation where

import qualified Test.SimpleTest.Color as Color
import Text.Printf

class Show e => Expectation e where
  holds :: e -> Bool

data EqualityAssertion where
  EqualityAssertion :: (Show a, Eq a) => a -> a -> EqualityAssertion

shouldBe :: (Show a, Eq a) => a -> a -> EqualityAssertion
shouldBe = EqualityAssertion

instance Expectation EqualityAssertion where
  holds (EqualityAssertion lhs rhs) = lhs == rhs

instance Show EqualityAssertion where
  show (EqualityAssertion lhs rhs) =
    printf "  Actual:   %s\n  Expected: %s" (show lhs) (show rhs)

data BooleanAssertion = BooleanAssertion Bool String

instance Show BooleanAssertion where
  show (BooleanAssertion b msg) =
    printf "  Assertion failed: %s" msg

instance Expectation BooleanAssertion where
  holds (BooleanAssertion b _) = b

shouldBeTrue :: Bool -> String -> BooleanAssertion
shouldBeTrue b msg = BooleanAssertion b msg

shouldBeFalse :: Bool -> String -> BooleanAssertion
shouldBeFalse b msg = BooleanAssertion (not b) msg

data PredicateAssertion where
  PredicateAssertion :: (Show a) => (a -> Bool) -> String -> a -> PredicateAssertion

instance Show PredicateAssertion where
  show (PredicateAssertion p msg v) =
    printf
      "  Actual: %s \n  %s: %s"
      (show v)
      (Color.yellow "does not satisfy predicate")
      msg

instance Expectation PredicateAssertion where
  holds (PredicateAssertion p msg v) = p v

shouldHold :: (Show a) => (a -> Bool) -> a -> PredicateAssertion
shouldHold p v = PredicateAssertion p "" v

shouldNotHold :: (Show a) => (a -> Bool) -> a -> PredicateAssertion
shouldNotHold p v = PredicateAssertion (not . p) "" v

withMessage :: PredicateAssertion -> String -> PredicateAssertion
withMessage (PredicateAssertion p _ v) msg = PredicateAssertion p msg v