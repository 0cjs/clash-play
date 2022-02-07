{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnitTest (unitTestMain) where

import Prelude
import Test.Framework
import {-@ HTF_TESTS @-} Hello
import {-@ HTF_TESTS @-} C3_1

unitTestMain :: IO ()
unitTestMain = htfMain htf_importedTests
