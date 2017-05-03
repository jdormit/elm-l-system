import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict
import List exposing (concatMap)
import Maybe exposing (withDefault)


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
    Dict.Dict Char (List Char)


type alias Model =
    { angle : Float
    , svgWidth : Float
    , svgHeight : Float
    , lineDelta : Float
    , angleDelta : Float
    , iterations: Int
    , axiom : List Char
    , rules : Rules
    }

initialModel : Model
initialModel =
    { angle = 45
    , svgWidth = 800
    , svgHeight = 800
    , lineDelta = 30.0
    , angleDelta = 45.0
    , iterations = 10
    , axiom = [ 'F', 'X' ]
    , rules = Dict.fromList [ ('X', String.toList "[-FX]+FX")]
    }

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)


-- Update


type Msg
    = Iterate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Iterate ->
            ({ model | iterations = model.iterations + 1 }, Cmd.none)


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- View


iterateLSystem : Int -> Rules -> List Char -> List Char
iterateLSystem iterations rules str =
    if iterations == 0 then
        str
    else
        let
            newStr = concatMap (\c -> withDefault [c] (Dict.get c rules)) str
        in
            iterateLSystem (iterations - 1) rules newStr


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
        angleRadians = angle * (pi / 180)
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
             , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
             ] []



drawLSystem : Float -> Float -> Cursor -> List Char -> Svg Msg
drawLSystem w h initialCursor instructions =
    svg [width (toString w), height (toString h)] (drawLSystemHelper instructions initialCursor [] [])


drawLSystemHelper : List Char -> Cursor -> List Cursor -> List (Svg Msg)-> List (Svg Msg)
drawLSystemHelper instructions cursor cursorStack lineAcc =
    case (List.head instructions) of
        Nothing -> lineAcc
        Just 'F' ->
            let
                newCoords = coordsFromAngleAndLength cursor.coords cursor.angleDelta cursor.lineDelta
                newCursor = { cursor | coords = newCoords }
                newLine = drawLine cursor.coords cursor.angleDelta cursor.lineDelta
                newLineAcc = newLine :: lineAcc
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack newLineAcc
        Just 'G' ->
            let
                newCoords = coordsFromAngleAndLength cursor.coords cursor.angleDelta cursor.lineDelta
                newCursor = { cursor | coords = newCoords }
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack lineAcc
        Just '+' ->
            let
                newAngle = cursor.angle + cursor.angleDelta
                newCursor = { cursor | angle = newAngle }
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack lineAcc
        Just '-' ->
            let
                newAngle = cursor.angle - cursor.angleDelta
                newCursor = { cursor | angle = newAngle }
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor cursorStack lineAcc
        Just '[' ->
            let
                newCursorStack = cursor :: cursorStack
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) cursor newCursorStack lineAcc
        Just ']' ->
            let
                newCursor = withDefault cursor (List.head cursorStack)
                newCursorStack = withDefault [] (List.tail cursorStack)
            in
                drawLSystemHelper (withDefault [] (List.tail instructions)) newCursor newCursorStack lineAcc
        _ ->
            drawLSystemHelper (withDefault [] (List.tail instructions)) cursor cursorStack lineAcc

view : Model -> Html Msg
view model =
    let
        initialCursor =
            { coords = (model.svgWidth / 2, model.svgHeight)
            , angle = model.angle
            , lineDelta = model.lineDelta
            , angleDelta = model.angleDelta
            }
        svgHeight = model.svgHeight
        svgWidth = model.svgWidth
        lSystem = iterateLSystem model.iterations model.rules model.axiom
    in
        div [
            ]
            [ (drawLSystem svgWidth svgHeight initialCursor lSystem)
            ]
