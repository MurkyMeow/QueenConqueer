module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as Events
import Html exposing (Html, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type Cell
    = Empty
    | Wall


map : Array (Array Cell)
map =
    let
        ( e, w ) =
            ( Empty, Wall )
    in
    [ [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    ]
        |> List.map Array.fromList
        |> Array.fromList


cellAt : Int -> Int -> Cell
cellAt x y =
    Array.get y map
        |> Maybe.andThen (Array.get x)
        |> Maybe.withDefault Wall


type alias Model =
    { goingForward : Bool
    , goingBackward : Bool
    , rotatingLeft : Bool
    , rotatingRight : Bool
    , posX : Float
    , posY : Float
    , angle : Float
    , textures : Maybe Textures
    }


type Msg
    = FramePassed Float
    | KeyPressed Key
    | KeyReleased Key
    | TexturesError Error
    | TexturesLoaded Textures


type alias Textures =
    { wall : Texture
    }


fetchTextures : Cmd Msg
fetchTextures =
    [ "/static/wall.png"
    ]
        |> List.map
            (Texture.loadWith
                { defaultOptions
                    | magnify = Texture.nearest
                    , minify = Texture.nearest
                }
            )
        |> Task.sequence
        |> Task.andThen
            (\textures ->
                case textures of
                    wall :: _ ->
                        Task.succeed (Textures wall)

                    _ ->
                        Task.fail Texture.LoadError
            )
        |> Task.attempt
            (\result ->
                case result of
                    Err error ->
                        TexturesError error

                    Ok textures ->
                        TexturesLoaded textures
            )


type Key
    = Up
    | Down
    | Left
    | Right


keyDecoder : Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowDown" ->
                        Decode.succeed Down

                    "ArrowLeft" ->
                        Decode.succeed Left

                    "ArrowRight" ->
                        Decode.succeed Right

                    _ ->
                        Decode.fail ("Not interested in " ++ s)
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { goingForward = False
      , goingBackward = False
      , rotatingLeft = False
      , rotatingRight = False
      , posX = 0.5
      , posY = 0.5
      , angle = pi
      , textures = Nothing
      }
    , fetchTextures
    )


plane : WebGL.Mesh Vertex
plane =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0) (vec2 0 0), Vertex (vec3 1 -1 0) (vec2 1 0), Vertex (vec3 1 1 0) (vec2 1 1) )
        , ( Vertex (vec3 -1 -1 0) (vec2 0 0), Vertex (vec3 -1 1 0) (vec2 0 1), Vertex (vec3 1 1 0) (vec2 1 1) )
        ]


makeTransform : Vec3 -> Vec2 -> Float -> Mat4
makeTransform position size rotation =
    let
        transform =
            Mat4.makeTranslate position

        rotation_ =
            Mat4.makeRotate rotation (vec3 0 0 1)

        scale =
            Mat4.makeScale (vec3 (Vec2.getX size) (Vec2.getY size) 1)
    in
    Mat4.mul (Mat4.mul transform rotation_) scale


view : Model -> Html msg
view model =
    case model.textures of
        Just { wall } ->
            let
                w =
                    400

                h =
                    400

                pos =
                    vec3 model.posX 0 model.posY

                dir =
                    vec3 (cos model.angle) 0 (sin model.angle)

                perspective =
                    Mat4.mul
                        (Mat4.makePerspective 45 (w / h) 0.01 100)
                        (Mat4.makeLookAt pos (Vec3.add pos dir) Vec3.j)

                wallTranform =
                    makeTransform (vec3 0 0 0) (vec2 1 1) 0
            in
            WebGL.toHtml
                [ width w
                , height h
                , style "display" "block"
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    plane
                    { texture = wall, perspective = perspective, transform = wallTranform }
                ]

        Nothing ->
            text "Loading textures..."


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


type alias Attributes =
    Vertex


type alias Uniforms =
    { texture : Texture
    , perspective : Mat4
    , transform : Mat4
    }


type alias Varyings =
    { vcoord : Vec2
    }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;

        uniform mat4 perspective;
        uniform mat4 transform;

        varying vec2 vcoord;

        void main() {
            gl_Position = perspective * transform * vec4(position, 1.0);
            vcoord = coord;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        varying vec2 vcoord;

        void main () {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FramePassed delta ->
            let
                newModel =
                    step delta model
            in
            ( newModel, Cmd.none )

        KeyPressed Up ->
            ( { model | goingForward = True }, Cmd.none )

        KeyReleased Up ->
            ( { model | goingForward = False }, Cmd.none )

        KeyPressed Down ->
            ( { model | goingBackward = True }, Cmd.none )

        KeyReleased Down ->
            ( { model | goingBackward = False }, Cmd.none )

        KeyPressed Left ->
            ( { model | rotatingLeft = True }, Cmd.none )

        KeyReleased Left ->
            ( { model | rotatingLeft = False }, Cmd.none )

        KeyPressed Right ->
            ( { model | rotatingRight = True }, Cmd.none )

        KeyReleased Right ->
            ( { model | rotatingRight = False }, Cmd.none )

        TexturesLoaded textures ->
            ( { model | textures = Just textures }, Cmd.none )

        TexturesError _ ->
            ( model, Cmd.none )


step : Float -> Model -> Model
step delta model =
    let
        rotationSpeed =
            0.0025 * delta

        newAngle =
            case ( model.rotatingLeft, model.rotatingRight ) of
                ( True, False ) ->
                    model.angle - rotationSpeed

                ( False, True ) ->
                    model.angle + rotationSpeed

                _ ->
                    model.angle

        movementSpeed =
            0.0025 * delta

        ( newPosX, newPosY ) =
            case ( model.goingForward, model.goingBackward ) of
                ( True, False ) ->
                    attemptMove
                        ( model.posX + cos newAngle * movementSpeed
                        , model.posY + sin newAngle * movementSpeed
                        )
                        ( model.posX
                        , model.posY
                        )

                ( False, True ) ->
                    attemptMove
                        ( model.posX - cos newAngle * movementSpeed
                        , model.posY - sin newAngle * movementSpeed
                        )
                        ( model.posX
                        , model.posY
                        )

                _ ->
                    ( model.posX, model.posY )
    in
    { model | angle = newAngle, posX = newPosX, posY = newPosY }


attemptMove : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
attemptMove ( targetPosX, targetPosY ) ( posX, posY ) =
    ( if cellAt (floor targetPosX) (floor posY) == Wall then
        posX

      else
        targetPosX
    , if cellAt (floor posX) (floor targetPosY) == Wall then
        posY

      else
        targetPosY
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta FramePassed
        , Events.onKeyDown (Decode.map KeyPressed keyDecoder)
        , Events.onKeyUp (Decode.map KeyReleased keyDecoder)
        ]
