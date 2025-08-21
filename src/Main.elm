port module Main exposing (..)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, text)
import Html.Attributes as A
import Html.Events as A
import Html.Lazy as H
import Html.Style as S
import Http
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Task exposing (Task)
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), (<?>))
import Url.Parser.Query as UrlQ



---- HELPERS ------------------------------------------------------------------


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a


iif : Bool -> a -> a -> a
iif c a b =
    if c then
        a

    else
        b


commas : String -> String
commas =
    String.reverse
        >> String.toList
        >> List.indexedMap
            (\i c ->
                if i > 0 && modBy 3 i == 0 then
                    [ ',', c ]

                else
                    [ c ]
            )
        >> List.concat
        >> String.fromList
        >> String.reverse


round2 : Float -> Float
round2 =
    (*) 100 >> floor >> toFloat >> flip (/) 100


usd : Float -> String
usd amount =
    let
        ( intPart, decPart ) =
            case amount |> abs |> round2 |> String.fromFloat |> String.split "." of
                a :: b :: _ ->
                    ( a, String.left 2 <| String.padRight 2 '0' b )

                a :: [] ->
                    ( a, "00" )

                [] ->
                    ( "0", "00" )
    in
    iif (amount < 0) "-" "" ++ "$" ++ commas intPart ++ "." ++ decPart



---- PORTS --------------------------------------------------------------------


port requestRepo : String -> Cmd msg


port repoLoaded : (D.Value -> msg) -> Sub msg



---- MAIN ---------------------------------------------------------------------


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClick
        }



---- MODEL --------------------------------------------------------------------


type alias Model =
    {}



-- TODO: Model
-- TODO:   errors : List Error
-- TODO:   repos : List String
-- TODO:   hover : Set Tag
-- TODO:   form : Filters
-- TODO:   route : Filters
-- TODO:   repo : Maybe Repo
-- TODO:   claude : Claude
-- TODO:   jobs : Array Job
-- TODO:
-- TODO: Job
-- TODO:   dest : JobDest
-- TODO:   request : Claude.Request
-- TODO:   status : Remote Http.Error Claude.Response
-- TODO:
-- TODO: JobDest
-- TODO:   Summary Filters
-- TODO:   ShortName Filters
-- TODO:   Suggestions Filters
-- TODO:   KeyEvent Filters
-- TODO:
-- TODO: Repo
-- TODO:   commits : Dict Id Event
-- TODO:   authors : Dict Id Author
-- TODO:   tags : Dict String Id
-- TODO:   branches : Dict String Event
-- TODO:   files : Set String
-- TODO:   github : Github
-- TODO:   report : Report
-- TODO:
-- TODO: Claude
-- TODO:   auth : String
-- TODO:   model : Claude.Model
-- TODO:
-- TODO: Github
-- TODO:   issues : Dict Int Event
-- TODO:   events : Dict Id Event
-- TODO:   users : Dict Id Github.User
-- TODO:
-- TODO: Report
-- TODO:   summary : String
-- TODO:   suggestions : List Suggestion
-- TODO:   events : List Event
-- TODO:
-- TODO: Suggestion
-- TODO:   text : String
-- TODO:   prompt : String
-- TODO:
-- TODO: Tag : String
-- TODO:
-- TODO: Event
-- TODO:   url : Url
-- TODO:   start : Time
-- TODO:   end : Maybe Time
-- TODO:   insertions : Int
-- TODO:   deletions : Int
-- TODO:   tags : Set Tag  -- e.g. commits, authors, tags, branches, files
-- TODO:   summary : String
-- TODO:
-- TODO: Filters
-- TODO:   repo : String
-- TODO:   start : String
-- TODO:   end : String
-- TODO:   tags : Set Tag
--
-- TODO: allEvents model = List.concat [repo.commits, repo.github.issues, repo.github.events, repo.report.events]
--
-- TODO: eventVector event =
-- TODO:   [ start, end, end - start, insertions, deletions ]
-- TODO:   -- TODO: Compute "file/directory distance" for filenames.
-- TODO:   ++ List.map (\tag -> iif (Set.member tag event.tags) 1.0 0.0) (Set.fromList allTags)
--
-- TODO: clusters n = allEvents model |> Random.List.shuffle |> Random.map (KMeans.clusterBy eventVector n)
--
---- PARSER -------------------------------------------------------------------


repoDecoder : D.Decoder Repo
repoDecoder =
    D.fail "TODO"



---- INIT ---------------------------------------------------------------------


type alias Flags =
    {}


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nav =
    let
        filters =
            route url
    in
    ( model, requestRepo filters.repo )


route : Url -> Filters
route url =
    -- TODO: e.g. /ziglang/zig?start=20240401&end=20250401&tags=\>main,@sally#202404
    url
        |> UrlP.parse Debug.todo
        |> Maybe.withDefault {}



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChange Url
    | LinkClick Browser.UrlRequest



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    -- TODO: fetchGithubEvents
    -- TODO: fetchGithubUsers
    -- TODO: fetchGithubIssues
    -- TODO: JobTick
    -- TODO: repoLoaded
    Sub.batch
        []



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({} as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            let
                filters =
                    route url
            in
            ( { model | route = filters, repo = Nothing }
            , iif (model.route.repo == filters.repo) Cmd.none (requestRepo filters.repo)
            )

        LinkClick (Browser.Internal url) ->
            ( model, Nav.pushUrl model.nav (Url.toString url) )

        LinkClick (Browser.External url) ->
            ( model, Nav.load url )



-- TODO: update
-- TODO:   RepoUrlChanged url -> { model | repoUrl = url }
-- TODO:   RepoUrlSubmitted -> model, navPush model.repoUrl
-- TODO:   StartChanged t -> model, navPush "?start=..."
-- TODO:   EndChanged t -> model, navPush "?end=..."
-- TODO:   TagAdded -> model, navPush "?tags=..."
-- TODO:   TagExcluded -> model, navPush "?tags=..."
-- TODO:   TagRemoved -> model, navPush "?tags=..."
-- TODO:   ReportRequested -> { model | repo = { repo | report = Just Report.init } }, Cmd.batch [ clusters 10 |> Random.generate ReportTagClustered, clusters 100 |> Random.generate ReportEventClustered, Claude.summarize model.repo ]
-- TODO:   ClaudeModelChanged mod -> model, changeClaude { claude | model = mod }
-- TODO:   ClaudeAuthChanged auth -> model, changeClaude { claude | auth = auth }
-- TODO:   Hovered tags -> { model | hover = tags }
-- TODO:   RepoChanged repo -> { model | repo = repo }, fetchGithubEvents repo
-- TODO:   ClaudeChanged claude -> { model | claude = claude }
-- TODO:   GithubEventsFetched events -> ...
-- TODO:   JobTick -> ... -- if no jobs are processing, start a new one
-- TODO:   JobCompleted i res -> ...
--
---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view ({} as model) =
    let
        flex =
            H.div [ S.displayFlex, S.flexWrapWrap ]

        rows =
            H.div [ S.displayFlex, S.flexDirectionColumn ]

        cols =
            H.div [ S.displayFlex, S.flexDirectionRow ]

        -- TODO: filteredEvents = allEvents model |> List.filter (\event -> model.route.start <= event.start && event.end <= model.route.end && not (Set.isEmpty (Set.intersect model.route.tags event.tags)) )
        -- TODO:
        -- TODO: allTags = allEvents model |> List.map .tags |> List.foldl Set.union Set.empty
        -- TODO:
        -- TODO: filteredTags = filteredEvents |> List.map .tags |> List.concatMap Set.toList |> List.foldl (\k d -> Dict.update k (Maybe.withDefault 0 >> (+) 1 >> Just)) Dict.empty |> Dict.toList |> List.sortBy (Tuple.second >> negate) |> List.map Tuple.first
    in
    { title = "diggit"
    , body =
        []
    }



-- TODO: view
-- TODO:   aside
-- TODO:     header
-- TODO:       h1: a: DIGGIT.DEV
-- TODO:       h2: for architecture archaeologists
-- TODO:       flex
-- TODO:         a: by taylor.town
-- TODO:         a: view on github
-- TODO:     section
-- TODO:       form
-- TODO:         input[name=repo]
-- TODO:         button submit
-- TODO:       flex
-- TODO:         a: elm-lang/compiler
-- TODO:         a: ziglang/zig
-- TODO:         a: roc-lang/roc
-- TODO:         a: ...recent searches
-- TODO:     section
-- TODO:       rows
-- TODO:         histogram: filteredEvents
-- TODO:           y: 1
-- TODO:           x: createdAt
-- TODO:         cols
-- TODO:           input[type=datetime,name=start]
-- TODO:           input[type=datetime,name=end]
-- TODO:       rows
-- TODO:         flex
-- TODO:           button: x @jonsmith
-- TODO:           button: x >main
-- TODO:           button: x .tsx
-- TODO:           button: + /src
-- TODO:           button: + #bug
-- TODO:           button: + "TODO"
-- TODO:           form
-- TODO:             input[name=tag]
-- TODO:             button: submit
-- TODO:         flex
-- TODO:           button: x .json
-- TODO:           button: - /node_modules
-- TODO:           button: - >staging
-- TODO:           form
-- TODO:             input[name=tag]
-- TODO:             button: submit
-- TODO:     section
-- TODO:       rows
-- TODO:         cols
-- TODO:           select
-- TODO:             option: opus 4.1
-- TODO:             option: sonnet 4.1
-- TODO:             option: haiku 3.5
-- TODO:           input[name=api-key]
-- TODO:         cols
-- TODO:           span: (list.sum (list.map .tokens claude.history)) tokens
-- TODO:           span: $(list.sum (list.map .price claude.history))
-- TODO:   main
-- TODO:     cols
-- TODO:       rows
-- TODO:         cols
-- TODO:           p: ai summary
-- TODO:           flex
-- TODO:             a: remove extra dependencies
-- TODO:             a: reduce transparency
-- TODO:             a: plan next migration
-- TODO:         flex: filteredEvents
-- TODO:           div[min-width=[merge,release].includes(type)?16rem:8rem]
-- TODO:             a: fixed bug (#41)
-- TODO:             flex
-- TODO:               span: 2024-04-02
-- TODO:               span: +242 -180
-- TODO:               a: @jonsmith
-- TODO:               a: >main
-- TODO:               a: #12f0b7
-- TODO:             p: summary
-- TODO:       histogram (vertical): filteredEvents
-- TODO:         y: linesAdded - linesRemoved
-- TODO:         x: createdAt
-- TODO:
