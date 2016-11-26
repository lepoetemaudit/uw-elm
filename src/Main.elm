import Html exposing (..)
import Bread exposing (..)

main =
  Html.programWithFlags 
    { init = init
    , subscriptions = \x -> Sub.none
    , update = update
    , view = view
  }
  
type alias Model = {
    strings : Result String Strings
}

type alias CharNode = 
  { symbol : String
  , parent : Int
  , left : Int
  , right : Int
  }

type alias Strings = 
  { numNode : Int
  , nodes : List CharNode
  , stringBlockCount : Int
  , stringBlocks : List StringBlockIndex }

type alias StringBlockIndex =
  { blockId : Int
  , fileOffset : Int }

decodeBlockIndex : DecodeContext -> ( DecodeValue StringBlockIndex, DecodeContext )   
decodeBlockIndex =
  map2 StringBlockIndex uint16 uint32

decodeCharNode : DecodeContext -> ( DecodeValue CharNode, DecodeContext )
decodeCharNode =
  map4 CharNode char uint8 uint8 uint8
  
decodeStrings flags = 
  ( read flags <| uint16 >>=
    (repeat decodeCharNode) >>= \nodes ->
    uint16 >>= \stringBlockCount ->
    (repeat decodeBlockIndex stringBlockCount) >>= \stringBlocks ->
       succeed { numNode = 0, nodes = nodes, stringBlockCount = stringBlockCount, stringBlocks = stringBlocks } )
       
  |> toResult
  

init : List Int -> ( Model, Cmd a)
init flags =
    ( { strings = decodeStrings flags }, Cmd.none )

update msg model =
    ( model ! [] )

view model =
    Html.p [] [ text <| toString model ]
