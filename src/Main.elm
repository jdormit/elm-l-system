import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict
import List exposing (concatMap)
import Maybe exposing (withDefault)
import Result
import String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Model


type alias Rules =
    Dict.Dict String (List String)


type alias Model =
    { angle : Float
    , svgWidth : Float
    , svgHeight : Float
    , lineDelta : Float
    , iterations: Int
    , instructions : List String
    , rules : Rules
    }

initialModel : Model
initialModel =
    { angle = 45.0
    , svgWidth = 800
    , svgHeight = 800
    , lineDelta = 150.0
    , iterations = 0
    , instructions = [ "F", "X" ]
    , rules = Dict.fromList [ ("X", ["#", ".6", "[", "-", "F", "X", "]", "+", "F", "X"])]
    }

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)


-- Update


iterateLSystem : Int -> Rules -> List String -> List String
iterateLSystem iterations rules instructions =
    if iterations == 0 then
        instructions
    else
        let
            newInstructions = concatMap (\c -> withDefault [c] (Dict.get c rules)) instructions
        in
            iterateLSystem (iterations - 1) rules newInstructions


type Msg
    = Iterate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Iterate ->
            ({ model
             | iterations = model.iterations + 1
             , instructions = (iterateLSystem (model.iterations + 1) model.rules model.instructions)
             }, Cmd.none)


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- View


type alias Cursor =
    { coords : (Float, Float)
    , angle : Float
    , lineDelta : Float
    , angleDelta : Float
    }


coordsFromAngleAndLength : (Float, Float) -> Float -> Float -> (Float, Float)
coordsFromAngleAndLength coords angle length =
    let
        (x, y) = coords
        angleRadians = degrees angle
    in
        (x + (length * cos angleRadians), y - (length * sin angleRadians))


drawLine : (Float, Float) -> Float -> Float -> Svg Msg
drawLine startCoords angle length =
    let
        (startX, startY) = startCoords
        (endX, endY) = coordsFromAngleAndLength startCoords angle length
    in
        line [ x1 (toString startX)
             , y1 (toString startY)
             , x2 (toString endX)
             , y2 (toString endY)
             , Svg.Attributes.style "stroke:rgb(0,128,0);stroke-width:2"
             ] []



drawLSystem : Float -> Float -> Cursor -> List String -> Svg Msg
drawLSystem w h initialCursor instructions =
    svg [ Svg.Attributes.width (toString w)
        , Svg.Attributes.height (toString h)
        ] (drawLSystemHelper instructions initialCursor [] [])


drawLSystemHelper : List String -> Cursor -> List Cursor -> List (Svg Msg)-> List (Svg Msg)
drawLSystemHelper instructions cursor cursorStack lineAcc =
    case (List.head instructions) of
        Nothing -> lineAcc
        Just "F" ->
            let
                newCoords = coordsFromAngleAndLength cursor.coords cursor.angle cursor.lineDelta
                newCursor = { cursor | coords = newCoords }
                newLine = drawLine cursor.coords cursor.angle cursor.lineDelta
                newLineAcc = newLine :: lineAcc
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack newLineAcc
        Just "G" ->
            let
                newCoords = coordsFromAngleAndLength cursor.coords cursor.angle cursor.lineDelta
                newCursor = { cursor | coords = newCoords }
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack lineAcc
        Just "+" ->
            let
                newAngle = cursor.angle + cursor.angleDelta
                newCursor = { cursor | angle = newAngle }
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack lineAcc
        Just "-" ->
            let
                newAngle = cursor.angle - cursor.angleDelta
                newCursor = { cursor | angle = newAngle }
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack lineAcc
        Just "[" ->
            let
                newCursorStack = cursor :: cursorStack
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) cursor newCursorStack lineAcc
        Just "]" ->
            let
                newCursor = withDefault cursor (List.head cursorStack)
                newCursorStack = withDefault [] (List.tail cursorStack)
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor newCursorStack lineAcc
        Just "#" ->
            let
                multiplier =
                    case List.head (withDefault [] (List.tail instructions)) of
                        Just str -> Result.withDefault 1.0 (String.toFloat str)
                        Nothing -> 1.0
                newInstructions = (withDefault [] (List.tail (withDefault [] (List.tail instructions))))
                newLineDelta = cursor.lineDelta * multiplier
                newCursor = { cursor | lineDelta = newLineDelta }
            in
                drawLSystemHelper newInstructions newCursor cursorStack lineAcc
        Just "@" ->
            let
                multiplier =
                    case List.head (withDefault [] (List.tail instructions)) of
                        Just str -> Result.withDefault 1.0 (String.toFloat str)
                        Nothing -> 1.0
                newInstructions = (withDefault [] (List.tail (withDefault [] (List.tail instructions))))
                newAngleDelta = cursor.angleDelta * multiplier
                newCursor = { cursor | angleDelta = newAngleDelta }
            in
                drawLSystemHelper newInstructions newCursor cursorStack lineAcc
 
        _ ->
            drawLSystemHelper (withDefault [] (List.tail instructions)) cursor cursorStack lineAcc

view : Model -> Html Msg
view model =
    let
        initialCursor =
            { coords = (model.svgWidth / 2, model.svgHeight)
            , angle = 90.0
            , lineDelta = model.lineDelta
            , angleDelta = model.angle
            }
        svgHeight = model.svgHeight
        svgWidth = model.svgWidth
    in
        div [
            ]
            [ (drawLSystem svgWidth svgHeight initialCursor model.instructions)
            , button [ onClick Iterate ] [ Html.text "Iterate L-System" ]
            ]
