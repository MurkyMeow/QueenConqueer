module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as Events
import Html exposing (Html)
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
    , objects : List GameObject
    }


type alias GameObject =
    { mesh : WebGL.Mesh Vertex
    , texture : Texture
    , position : Vec2
    , rotation : Float
    , rotationAxis : Vec3
    , size : Vec2
    , sizeAxis : Vec3
    }


type Msg
    = FramePassed Float
    | KeyPressed Key
    | KeyReleased Key
    | TexturesError Error
    | TexturesLoaded Textures


type alias Textures =
    { wall : Texture
    , tree : Texture
    }


fetchTextures : Cmd Msg
fetchTextures =
    [ "/static/wall.png"
    , "/static/tree.png"
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
                    wall :: tree :: _ ->
                        Task.succeed (Textures wall tree)

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
      , objects = []
      }
    , fetchTextures
    )


topLeft : Vec2
topLeft =
    vec2 0 0


topRight : Vec2
topRight =
    vec2 1 0


bottomRight : Vec2
bottomRight =
    vec2 1 1


bottomLeft : Vec2
bottomLeft =
    vec2 0 1


plane : WebGL.Mesh Vertex
plane =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0) topLeft, Vertex (vec3 1 -1 0) topRight, Vertex (vec3 1 1 0) bottomRight )
        , ( Vertex (vec3 -1 -1 0) topLeft, Vertex (vec3 -1 1 0) bottomLeft, Vertex (vec3 1 1 0) bottomRight )
        ]


treeMesh : WebGL.Mesh Vertex
treeMesh =
    let
        rot45 =
            Mat4.transform (Mat4.makeRotate 1 (vec3 0 1 0))

        rot90 =
            Mat4.transform (Mat4.makeRotate 3 (vec3 0 1 0))
    in
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0) topLeft, Vertex (vec3 1 -1 0) topRight, Vertex (vec3 1 1 0) bottomRight )
        , ( Vertex (vec3 -1 -1 0) topLeft, Vertex (vec3 -1 1 0) bottomLeft, Vertex (vec3 1 1 0) bottomRight )
        , ( Vertex (rot45 (vec3 -1 -1 0)) topLeft, Vertex (rot45 (vec3 1 -1 0)) topRight, Vertex (rot45 (vec3 1 1 0)) bottomRight )
        , ( Vertex (rot45 (vec3 -1 -1 0)) topLeft, Vertex (rot45 (vec3 -1 1 0)) bottomLeft, Vertex (rot45 (vec3 1 1 0)) bottomRight )
        , ( Vertex (rot90 (vec3 -1 -1 0)) topLeft, Vertex (rot90 (vec3 1 -1 0)) topRight, Vertex (rot90 (vec3 1 1 0)) bottomRight )
        , ( Vertex (rot90 (vec3 -1 -1 0)) topLeft, Vertex (rot90 (vec3 -1 1 0)) bottomLeft, Vertex (rot90 (vec3 1 1 0)) bottomRight )
        ]


makeTransform : Vec3 -> Vec2 -> Float -> Vec3 -> Vec3 -> Mat4
makeTransform position size rotation rotationAxis sizeAxis =
    let
        transform =
            Mat4.makeTranslate position

        rotTranslation =
            Mat4.makeTranslate rotationAxis

        rotTranslationInv =
            let
                inv =
                    Mat4.inverse rotTranslation
            in
            case inv of
                Just mat ->
                    mat

                Nothing ->
                    Mat4.identity

        scaleTranslation =
            Mat4.makeTranslate sizeAxis

        scaleTranslationInv =
            let
                inv =
                    Mat4.inverse scaleTranslation
            in
            case inv of
                Just mat ->
                    mat

                Nothing ->
                    Mat4.identity

        rotation_ =
            Mat4.mul rotTranslationInv (Mat4.mul (Mat4.makeRotate rotation (vec3 0 1 0)) rotTranslation)

        scale3d =
            vec3 (Vec2.getX size) (Vec2.getY size) 1

        scale =
            Mat4.mul scaleTranslationInv (Mat4.mul (Mat4.makeScale scale3d) scaleTranslation)
    in
    Mat4.mul (Mat4.mul transform rotation_) scale


view : Model -> Html msg
view model =
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
    in
    WebGL.toHtml
        [ width w
        , height h
        , style "display" "block"
        ]
        (List.map (gameObjectToEntity perspective) model.objects)


gameObjectToEntity : Mat4 -> GameObject -> WebGL.Entity
gameObjectToEntity perspective obj =
    let
        pos3d =
            vec3 (Vec2.getX obj.position) 0 (Vec2.getY obj.position)

        transform =
            makeTransform pos3d obj.size obj.rotation obj.rotationAxis obj.sizeAxis
    in
    WebGL.entity
        vertexShader
        fragmentShader
        obj.mesh
        { texture = obj.texture, perspective = perspective, transform = transform }


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

            // make the transparency work
            if (gl_FragColor.a == 0.0) {
                discard;
            }
        }
    |]


scene1Objects : Textures -> List GameObject
scene1Objects { wall, tree } =
    [ GameObject plane wall (vec2 0 0) 0 (vec3 0 0 0) (vec2 1 1) (vec3 0 0 0)
    , GameObject plane wall (vec2 2 0) -1 (vec3 1 0 0) (vec2 1 1) (vec3 0 0 0)
    , GameObject plane wall (vec2 3 0) 1 (vec3 -1 0 0) (vec2 1 1) (vec3 0 0 0)
    , GameObject plane wall (vec2 5 0) 0 (vec3 0 0 0) (vec2 1 1) (vec3 0 0 0)
    , GameObject treeMesh tree (vec2 2 0) 0 (vec3 0 0 0) (vec2 1 3) (vec3 0 1 0)
    ]


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
            ( { model | objects = scene1Objects textures }, Cmd.none )

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
                    ( model.posX + cos newAngle * movementSpeed
                    , model.posY + sin newAngle * movementSpeed
                    )

                ( False, True ) ->
                    ( model.posX - cos newAngle * movementSpeed
                    , model.posY - sin newAngle * movementSpeed
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
