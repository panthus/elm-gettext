module MoFileParser exposing (MoFile, parser)

import Array exposing (Array)
import Bitwise
import Bytes as B
import Bytes.Decode as D
import Dict exposing (Dict)
import PluralFormsParser exposing (PluralForms)



{- Parse a MO file as specified here: https://www.gnu.org/software/gettext/manual/html_node/MO-Files.html.
   We require that the MO file has the Language and Plural-Forms header.
-}


type alias MoFile =
    { headers : Dict String String
    , pluralForms : PluralForms
    , language : String
    , translations : Dict String (Array String)
    }


parser : D.Decoder MoFile
parser =
    detectEndian
        |> D.andThen
            (\endian ->
                decodeHeader endian
                    |> D.andThen
                        (\header ->
                            let
                                -- Header is 28 bytes
                                toBeConsumedToOrigin =
                                    header.origOffset - 28

                                -- TableEntry is 8 bytes
                                toBeConsumedToTranslations =
                                    header.transOffset - (header.numStrings * 8) - 28

                                consumedAfterTranslations =
                                    header.transOffset + (header.numStrings * 8)
                            in
                            D.map2 Tuple.pair
                                (decodeTable toBeConsumedToOrigin header.numStrings 8 (decodeTableEntry endian))
                                (decodeTable toBeConsumedToTranslations header.numStrings 8 (decodeTableEntry endian))
                                |> D.andThen
                                    (\( originalTable, translationTable ) ->
                                        decodeStrings consumedAfterTranslations originalTable
                                            |> D.andThen
                                                (\( consumedBytes, original ) ->
                                                    D.andThen
                                                        (\( _, translation ) ->
                                                            let
                                                                translations =
                                                                    List.map2 decodeTranslation original translation
                                                                        |> Dict.fromList

                                                                headers =
                                                                    translations
                                                                        |> Dict.get ""
                                                                        |> Maybe.andThen (Array.get 0)
                                                                        |> Maybe.map parseHeaders
                                                                        |> Maybe.withDefault Dict.empty
                                                            in
                                                            Maybe.map2
                                                                (\p l ->
                                                                    D.succeed
                                                                        { headers = headers
                                                                        , pluralForms = p
                                                                        , language = l
                                                                        , translations = translations |> Dict.remove ""
                                                                        }
                                                                )
                                                                (headers
                                                                    |> Dict.get "Plural-Forms"
                                                                    |> Maybe.andThen (PluralFormsParser.parse >> Result.toMaybe)
                                                                )
                                                                (Dict.get "Language" headers)
                                                                |> Maybe.withDefault D.fail
                                                        )
                                                        (decodeStrings consumedBytes translationTable)
                                                )
                                    )
                        )
            )


parseHeaders : String -> Dict String String
parseHeaders headers =
    headers
        |> String.split "\n"
        |> List.foldl
            (\h r ->
                h
                    |> String.indexes ":"
                    |> List.head
                    |> Maybe.map
                        (\i ->
                            Dict.insert (h |> String.slice 0 i)
                                (h |> String.slice (i + 1) (String.length h) |> String.trim)
                                r
                        )
                    |> Maybe.withDefault r
            )
            Dict.empty


detectEndian : D.Decoder B.Endianness
detectEndian =
    D.unsignedInt32 B.LE
        |> D.andThen
            (\magic ->
                case magic of
                    0x950412DE ->
                        D.succeed B.LE

                    0xDE120495 ->
                        D.succeed B.BE

                    _ ->
                        D.fail
            )


type alias Revision =
    { major : Int, minor : Int }


decodeRevision : B.Endianness -> D.Decoder Revision
decodeRevision endian =
    D.unsignedInt32 endian
        |> D.andThen
            (\rev ->
                let
                    major =
                        Bitwise.shiftRightZfBy 16 rev

                    minor =
                        Bitwise.and 0xFFFF rev
                in
                if major == 0 then
                    D.succeed { major = major, minor = minor }

                else
                    D.fail
            )


type alias Header =
    { revision : Revision
    , numStrings : Int
    , origOffset : Int
    , transOffset : Int
    , hashSize : Int
    , hashOffset : Int
    }


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap aDecoder fnDecoder =
    D.map2 (<|) fnDecoder aDecoder


decodeHeader : B.Endianness -> D.Decoder Header
decodeHeader endian =
    D.succeed Header
        |> andMap (decodeRevision endian)
        |> andMap (D.unsignedInt32 endian)
        |> andMap (D.unsignedInt32 endian)
        |> andMap (D.unsignedInt32 endian)
        |> andMap (D.unsignedInt32 endian)
        |> andMap (D.unsignedInt32 endian)


type alias TableEntry =
    { length : Int, offset : Int }


decodeTableEntry : B.Endianness -> D.Decoder TableEntry
decodeTableEntry endian =
    D.map2 TableEntry
        (D.unsignedInt32 endian)
        (D.unsignedInt32 endian)


decodeTable : Int -> Int -> Int -> D.Decoder a -> D.Decoder (List a)
decodeTable startOffset numEntries stepSize decoder =
    D.bytes startOffset
        |> D.andThen (\_ -> D.loop ( numEntries * stepSize, [] ) (step stepSize decoder))


step : Int -> D.Decoder a -> ( Int, List a ) -> D.Decoder (D.Step ( Int, List a ) (List a))
step stepSize decoder ( sizeInBytes, xs ) =
    if sizeInBytes <= 0 then
        D.succeed (xs |> List.reverse |> D.Done)

    else
        D.map (\x -> D.Loop ( sizeInBytes - stepSize, x :: xs )) decoder


{-| Returns a tuple containing a key and the translations.
The key is one of the following depending on if there is context or not:

  - the singular original string
  - the context + EOT byte (\\u{0004}) + singular original string.

-}
decodeTranslation : String -> String -> ( String, Array String )
decodeTranslation original translation =
    let
        toStrings strings =
            strings |> String.split "\u{0000}" |> Array.fromList
    in
    ( original |> toStrings |> Array.get 0 |> Maybe.withDefault "", toStrings translation )


decodeStrings : Int -> List TableEntry -> D.Decoder ( Int, List String )
decodeStrings consumedBytes table =
    D.loop ( consumedBytes, table, [] ) stringStep


stringStep :
    ( Int, List TableEntry, List String )
    -> D.Decoder (D.Step ( Int, List TableEntry, List String ) ( Int, List String ))
stringStep ( consumedBytes, table, result ) =
    case table of
        [] ->
            ( consumedBytes, result ) |> D.Done |> D.succeed

        entry :: nextTable ->
            let
                toConsume =
                    entry.offset - consumedBytes
            in
            D.bytes toConsume
                |> D.andThen
                    (\_ ->
                        D.map (\x -> D.Loop ( consumedBytes + toConsume + entry.length, nextTable, x :: result ))
                            (D.string entry.length)
                    )
