module XmlSpec where

import Test.Hspec
import Xml
import Parser

spec :: Spec
spec =
  describe "node" $ do
    it "parses a simple xml string" $ do
      let input = "<test foo=\"bar\"><baz>ramsam</baz></test>"
      let output = TagNode { name = "test"
                           , attributes = [ Attribute { key = "foo", value = "bar" } ]
                           , children =
                             [ TagNode { name = "baz"
                                       , attributes = []
                                       , children = [ TextNode { content = "ramsam" } ]
                                       }
                             ]
                           }
      parse node input `shouldBe` [(output, "")]

    it "parses a simple xml string with spaces & newlines" $ do
      let input = "<test foo=\"bar\">\
\    <baz>\
\        ramsam\
\    </baz>\
\</test>"
      let output = TagNode { name = "test"
                           , attributes = [ Attribute { key = "foo", value = "bar" } ]
                           , children =
                             [ TagNode { name = "baz"
                                       , attributes = []
                                       , children = [ TextNode { content = "ramsam" } ]
                                       }
                             ]
                           }
      parse node input `shouldBe` [(output, "")]
