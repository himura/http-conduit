{-# LANGUAGE OverloadedStrings #-}

module ParserTest (parserTest) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.Expectations
import Network.HTTP.Conduit.Parser
import qualified Data.Attoparsec as A

parserTest :: Spec
parserTest = do
    describe "parseStatus" $ do
        let parse = A.parseOnly parseStatus
        it "could parse well-formed STATUS-LINE" $
            parse "HTTP/1.1 200 OK\n" `shouldBe` Right ("1.1", 200, "OK")
        it "could parse STATUS-LINE with empty Reason-Phrase" $
            parse "HTTP/1.1 423 \n" `shouldBe` Right ("1.1", 423, "")
        it "could parse STATUS-LINE which is lacking of the separator" $
            parse "HTTP/1.1 423\n" `shouldBe` Right ("1.1", 423, "")
