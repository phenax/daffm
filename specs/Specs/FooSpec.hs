module Specs.FooSpec where

import Daffm.Action.Commands (parseCommand)
import Daffm.Configuration (parseKey)
import Daffm.Event (matchKeySequence)
import Daffm.Types
import qualified Data.Map as Map
import qualified Graphics.Vty as K
import Test.Hspec

test :: SpecWith ()
test = do
  describe "matchKeySequence" $ do
    context "when key sequence is empty" $ do
      it "returns MatchFailure" $ do
        let keymap = Map.fromList [([K.KChar 'a', K.KChar 'b', K.KChar 'c'], CmdNoop)]
        matchKeySequence keymap [] `shouldBe` MatchFailure

    context "when keymap is empty" $ do
      it "returns MatchFailure" $ do
        matchKeySequence Map.empty [K.KChar 'a'] `shouldBe` MatchFailure

    context "when key sequence does not match any of the keymaps" $ do
      it "returns MatchPartial" $ do
        let keymap =
              Map.fromList
                [ ([K.KChar 'a', K.KChar 'b', K.KChar 'c'], CmdShell True "1"),
                  ([K.KChar 'a', K.KChar 'd', K.KChar 'e'], CmdShell True "2")
                ]
        let keys = [K.KChar 'a', K.KChar 'x']
        matchKeySequence keymap keys `shouldBe` MatchFailure

    context "when key sequence partially matches one of the keymaps" $ do
      it "returns MatchPartial" $ do
        let keymap =
              Map.fromList
                [ ([K.KChar 'a', K.KChar 'b', K.KChar 'c'], CmdShell True "1"),
                  ([K.KChar 'a', K.KChar 'd', K.KChar 'e'], CmdShell True "2")
                ]
        let keys = [K.KChar 'a', K.KChar 'b']
        matchKeySequence keymap keys `shouldBe` MatchPartial

    context "when key sequence matches with one of the keymaps" $ do
      it "returns MatchPartial" $ do
        let keymap =
              Map.fromList
                [ ([K.KChar 'a', K.KChar 'b', K.KChar 'c'], CmdShell True "1"),
                  ([K.KChar 'a', K.KChar 'd', K.KChar 'e'], CmdShell True "2")
                ]
        let keys = [K.KChar 'a', K.KChar 'd', K.KChar 'e']
        matchKeySequence keymap keys `shouldBe` MatchSuccess (CmdShell True "2")

  describe "parseCommand" $ do
    context "when given an invalid command" $ do
      it "returns Nothing" $ do
        parseCommand "aklsdjijm" `shouldBe` Nothing
    context "when empty command" $ do
      it "returns Nothing" $ do
        parseCommand "" `shouldBe` Nothing

    context "when command prefixed with !" $ do
      it "parses as shell command without wait for key" $ do
        parseCommand "!ls -la /" `shouldBe` Just (CmdShell False "ls -la /")

    context "when command prefixed with !!" $ do
      it "parses as shell command with wait for key" $ do
        parseCommand "!!ls -la /" `shouldBe` Just (CmdShell True "ls -la /")

    context "when given quit" $ do
      it "parses correctly" $ do
        parseCommand "quit invalid args" `shouldBe` Just CmdQuit
        parseCommand "quit" `shouldBe` Just CmdQuit

    context "when given set-cmdline" $ do
      it "parses correctly" $ do
        parseCommand "cmdline-set hello" `shouldBe` Just (CmdSetCmdline "hello")
        parseCommand "cmdline-set" `shouldBe` Just (CmdSetCmdline "")
        parseCommand "cmdline-set somespaces    " `shouldBe` Just (CmdSetCmdline "somespaces    ")

  describe "parseKey" $ do
    context "when given keys" $ do
      it "parses correctly" $ do
        parseKey "gdl" `shouldBe` Just [K.KChar 'g', K.KChar 'd', K.KChar 'l']
        parseKey "<tab>g<cr>" `shouldBe` Just [K.KChar '\t', K.KChar 'g', K.KEnter]
        parseKey "<esc>22" `shouldBe` Just [K.KEsc, K.KChar '2', K.KChar '2']
