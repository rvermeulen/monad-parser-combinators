module MonadicParserCombinatorsTest where

import qualified MonadicParserCombinators as PC
import Test.HUnit

item :: Test
item = "item" ~: (PC.runParser PC.item) "abc" ~?= [('a', "bc")]


tests :: [Test]
tests = [item]
