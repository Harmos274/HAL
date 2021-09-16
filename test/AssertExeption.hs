module AssertExeption
  ( assertHException
  ) where

import Test.HUnit (assertFailure, Assertion)
import Control.Exception (handleJust)
import Control.Monad (guard)

import Exception (HExceptions (..))

newtype TestException = TestException HExceptions deriving (Show)

instance Eq TestException where
    TestException (ArgumentException   s) == TestException (ArgumentException   s2) = s == s2
    TestException (ParsingException    s) == TestException (ParsingException    s2) = s == s2
    TestException (LexingException     s) == TestException (LexingException     s2) = s == s2
    TestException (EvaluationException s) == TestException (EvaluationException s2) = s == s2
    _                                     == _                                        = False

assertHException :: HExceptions -> IO a -> Assertion
assertHException ex action = handleJust handler (const $ return ()) $
        do  _ <- action
            assertFailure $ "Expected exception: " ++ show ex
        where handler :: HExceptions -> Maybe ()
              handler e = guard (TestException e == TestException ex)

