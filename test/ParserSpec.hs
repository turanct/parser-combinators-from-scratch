module ParserSpec where

import Test.Hspec
import Parser

spec :: Spec
spec = do
  describe "char" $ do
    it "matches a single character" $
      parse (char 'x') "xa" `shouldBe` [('x', "a")]

    it "does not match a single character" $
      parse (char 'x') "ax" `shouldBe` []

  describe "string" $ do
    it "matches a given string" $
      parse (string "foobar") "foobarbazqux" `shouldBe` [("foobar", "bazqux")]

    it "does not match a given string" $
      parse (string "foobar") "bazqux" `shouldBe` []

  describe "option" $ do
    it "matches if the first option matches" $
      parse (option (char 'x') (char 'y')) "xxxx" `shouldBe` [('x', "xxx")]

    it "matches if the second option matches" $
      parse (option (char 'x') (char 'y')) "yxxx" `shouldBe` [('y', "xxx")]

    it "does not match if none of the options match" $
      parse (option (char 'x') (char 'y')) "aaaa" `shouldBe` []

  describe "many" $ do
    it "matches a given parser once" $
      parse (many (char 'x')) "xaaa" `shouldBe` [("x", "aaa")]

    it "matches a given parser lots of times" $
      parse (many (char 'x')) "xxxa" `shouldBe` [("xxx", "a")]

    it "matches a given parser zero times" $
      parse (many (char 'x')) "aaaa" `shouldBe` [("", "aaaa")]

  describe "many1" $ do
    it "matches a given parser once" $
      parse (many1 (char 'x')) "xaaa" `shouldBe` [("x", "aaa")]

    it "matches a given parser lots of times" $
      parse (many1 (char 'x')) "xxxa" `shouldBe` [("xxx", "a")]

    it "does not match a given parser zero times" $
      parse (many1 (char 'x')) "aaaa" `shouldBe` []

  describe "not" $ do
    it "matches if parser does not" $
      parse (nope (char 'x')) "a" `shouldBe` [('a', "")]

    it "does not match if parser matches" $
      parse (nope (char 'x')) "x" `shouldBe` []

  describe "surroundedBy" $ do
    it "matches the text surrounded by a given parser" $
      parse (surroundedBy (char 'x')) "xax" `shouldBe` [("a", "")]

    it "matches multiple chars surrounded by a given parser" $
      parse (surroundedBy (char 'x')) "xaaax" `shouldBe` [("aaa", "")]

    it "matches if there's nothing in between" $
      parse (surroundedBy (char 'x')) "xx" `shouldBe` [("", "")]

    it "does not match if there's nothing to match" $ do
      parse (surroundedBy (char 'x')) "a" `shouldBe` []
      parse (surroundedBy (char 'x')) "x" `shouldBe` []

  describe "surroundedBy1" $ do
    it "matches the text surrounded by a given parser" $
      parse (surroundedBy1 (char 'x')) "xax" `shouldBe` [("a", "")]

    it "matches multiple chars surrounded by a given parser" $
      parse (surroundedBy1 (char 'x')) "xaaax" `shouldBe` [("aaa", "")]

    it "does not match if there's nothing in between" $
      parse (surroundedBy1 (char 'x')) "xx" `shouldBe` []

    it "does not match if there's nothing to match" $ do
      parse (surroundedBy1 (char 'x')) "a" `shouldBe` []
      parse (surroundedBy1 (char 'x')) "x" `shouldBe` []

  describe "whitespace" $ do
    it "matches a space" $
      parse whitespace " foo" `shouldBe` [(' ', "foo")]

    it "matches a tab" $
      parse whitespace "\tfoo" `shouldBe` [('\t', "foo")]

    it "does not match something else" $
      parse whitespace "foo" `shouldBe` []

  describe "eol" $ do
    it "matches a newline" $
      parse eol "\nfoo" `shouldBe` [('\n', "foo")]

    it "does not match something else" $
      parse whitespace "foo" `shouldBe` []
