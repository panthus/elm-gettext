module MoFileParserTest exposing (..)

import Bytes.Decode as D
import Expect
import MoFile
import MoFileParser
import PluralFormsParser exposing (Expr(..))
import Test exposing (Test)


suite : Test
suite =
    Test.test "Parse a MO file and verify the result" <|
        \_ ->
            Expect.equal (Just MoFile.parsedMoFile) (D.decode MoFileParser.parser MoFile.moFile)
