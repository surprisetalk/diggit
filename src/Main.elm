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


type alias Tag =
    String


type alias Id =
    String


type alias Time =
    Float


type alias Filters =
    { repo : String
    , start : String
    , end : String
    , tags : Set Tag
    }


type alias Event =
    { id : Id
    , url : String
    , start : Time
    , end : Maybe Time
    , insertions : Int
    , deletions : Int
    , tags : Set Tag
    , summary : String
    }


type alias Author =
    { id : Id
    , name : String
    , email : String
    , avatarUrl : Maybe String
    }


type alias GithubUser =
    { id : Id
    , login : String
    , name : Maybe String
    , avatarUrl : String
    , htmlUrl : String
    }


type alias Github =
    { issues : Dict Int Event
    , events : Dict Id Event
    , users : Dict Id GithubUser
    }


type alias Suggestion =
    { text : String
    , prompt : String
    }


type alias Report =
    { summary : String
    , suggestions : List Suggestion
    , events : List Event
    }


type alias Repo =
    { url : String
    , commits : Dict Id Event
    , authors : Dict Id Author
    , tags : Dict String Id
    , branches : Dict String Event
    , files : Set String
    , github : Github
    , report : Maybe Report
    }


type ClaudeModel
    = Opus41
    | Sonnet41
    | Haiku35


type alias ClaudeRequest =
    { prompt : String
    , model : ClaudeModel
    , maxTokens : Int
    }


type alias ClaudeResponse =
    { content : String
    , usage : { inputTokens : Int, outputTokens : Int }
    , model : ClaudeModel
    }


type alias Claude =
    { auth : String
    , model : ClaudeModel
    , history : List { request : ClaudeRequest, response : ClaudeResponse, tokens : Int, price : Float }
    }


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type JobDest
    = Summary Filters
    | ShortName Filters
    | Suggestions Filters
    | KeyEvent Filters


type alias Job =
    { dest : JobDest
    , request : ClaudeRequest
    , status : RemoteData Http.Error ClaudeResponse
    }


type alias Error =
    { message : String
    , timestamp : Time
    }


type alias Model =
    { nav : Nav.Key
    , errors : List Error
    , repos : List String
    , hover : Set Tag
    , form : Filters
    , route : Filters
    , repo : Maybe Repo
    , claude : Claude
    , jobs : Array Job
    }


allEvents : Model -> List Event
allEvents model =
    case model.repo of
        Nothing ->
            []

        Just repo ->
            List.concat
                [ Dict.values repo.commits
                , Dict.values repo.github.issues
                , Dict.values repo.github.events
                , case repo.report of
                    Nothing ->
                        []

                    Just report ->
                        report.events
                ]


eventVector : List Tag -> Event -> List Float
eventVector allTags event =
    let
        duration =
            case event.end of
                Nothing ->
                    0

                Just endTime ->
                    endTime - event.start
    in
    [ event.start
    , Maybe.withDefault event.start event.end
    , duration
    , toFloat event.insertions
    , toFloat event.deletions
    ]
        ++ List.map (\tag -> iif (Set.member tag event.tags) 1.0 0.0) allTags



-- TODO: clusters n = allEvents model |> Random.List.shuffle |> Random.map (KMeans.clusterBy eventVector n)
--
---- PARSER -------------------------------------------------------------------


repoDecoder : D.Decoder Repo
repoDecoder =
    D.map8 Repo
        (D.field "url" D.string)
        (D.field "commits" (D.dict eventDecoder))
        (D.field "authors" (D.dict authorDecoder))
        (D.field "tags" (D.dict D.string))
        (D.field "branches" (D.dict eventDecoder))
        (D.field "files" (D.list D.string |> D.map Set.fromList))
        (D.field "github" githubDecoder)
        (D.maybe (D.field "report" reportDecoder))


eventDecoder : D.Decoder Event
eventDecoder =
    D.map8 Event
        (D.field "id" D.string)
        (D.field "url" D.string)
        (D.field "start" D.float)
        (D.maybe (D.field "end" D.float))
        (D.field "insertions" D.int)
        (D.field "deletions" D.int)
        (D.field "tags" (D.list D.string |> D.map Set.fromList))
        (D.field "summary" D.string)


authorDecoder : D.Decoder Author
authorDecoder =
    D.map4 Author
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "email" D.string)
        (D.maybe (D.field "avatarUrl" D.string))


githubDecoder : D.Decoder Github
githubDecoder =
    D.map3 Github
        (D.field "issues"
            (D.dict eventDecoder
                |> D.map
                    (Dict.foldl
                        (\k v acc ->
                            case String.toInt k of
                                Just intKey ->
                                    Dict.insert intKey v acc

                                Nothing ->
                                    acc
                        )
                        Dict.empty
                    )
            )
        )
        (D.field "events" (D.dict eventDecoder))
        (D.field "users" (D.dict githubUserDecoder))


githubUserDecoder : D.Decoder GithubUser
githubUserDecoder =
    D.map5 GithubUser
        (D.field "id" D.string)
        (D.field "login" D.string)
        (D.maybe (D.field "name" D.string))
        (D.field "avatarUrl" D.string)
        (D.field "htmlUrl" D.string)


reportDecoder : D.Decoder Report
reportDecoder =
    D.map3 Report
        (D.field "summary" D.string)
        (D.field "suggestions" (D.list suggestionDecoder))
        (D.field "events" (D.list eventDecoder))


suggestionDecoder : D.Decoder Suggestion
suggestionDecoder =
    D.map2 Suggestion
        (D.field "text" D.string)
        (D.field "prompt" D.string)



---- INIT ---------------------------------------------------------------------


type alias Flags =
    { claudeAuth : Maybe String
    , claudeModel : Maybe String
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    let
        filters =
            route url

        initialClaude =
            { auth = Maybe.withDefault "" flags.claudeAuth
            , model =
                case flags.claudeModel of
                    Just "opus41" ->
                        Opus41

                    Just "sonnet41" ->
                        Sonnet41

                    Just "haiku35" ->
                        Haiku35

                    _ ->
                        Sonnet41
            , history = []
            }

        model =
            { nav = nav
            , errors = []
            , repos = [ "elm-lang/compiler", "ziglang/zig", "roc-lang/roc" ]
            , hover = Set.empty
            , form = filters
            , route = filters
            , repo = Nothing
            , claude = initialClaude
            , jobs = Array.empty
            }
    in
    ( model
    , if String.isEmpty filters.repo then
        Cmd.none

      else
        requestRepo filters.repo
    )


routeParser : UrlP.Parser (Filters -> a) a
routeParser =
    UrlP.map makeFilters
        (UrlP.s "repo"
            </> UrlP.string
            </> UrlP.string
            <?> UrlQ.string "start"
            <?> UrlQ.string "end"
            <?> UrlQ.string "tags"
        )


makeFilters : String -> String -> Maybe String -> Maybe String -> Maybe String -> Filters
makeFilters owner repo maybeStart maybeEnd maybeTags =
    { repo = owner ++ "/" ++ repo
    , start = Maybe.withDefault "" maybeStart
    , end = Maybe.withDefault "" maybeEnd
    , tags =
        maybeTags
            |> Maybe.withDefault ""
            |> String.split ","
            |> List.filter (not << String.isEmpty)
            |> Set.fromList
    }


route : Url -> Filters
route url =
    -- e.g. /repo/ziglang/zig?start=20240401&end=20250401&tags=\>main,@sally#202404
    url
        |> UrlP.parse routeParser
        |> Maybe.withDefault
            { repo = ""
            , start = ""
            , end = ""
            , tags = Set.empty
            }



---- MESSAGES -----------------------------------------------------------------


type Msg
    = NoOp
    | UrlChange Url
    | LinkClick Browser.UrlRequest
    | RepoUrlChanged String
    | RepoUrlSubmitted
    | StartChanged String
    | EndChanged String
    | TagAdded Tag
    | TagExcluded Tag
    | TagRemoved Tag
    | ReportRequested
    | ClaudeModelChanged ClaudeModel
    | ClaudeAuthChanged String
    | Hovered (Set Tag)
    | RepoLoaded D.Value
    | GithubEventsFetched (Result Http.Error (List Event))
    | GithubUsersFetched (Result Http.Error (List GithubUser))
    | GithubIssuesFetched (Result Http.Error (Dict Int Event))
    | JobTick Time
    | JobCompleted Int (Result Http.Error ClaudeResponse)
    | AddError String



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ repoLoaded RepoLoaded

        -- TODO: Time.every (1000) JobTick
        ]



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        RepoUrlChanged url ->
            let
                newForm =
                    model.form
                        |> (\f -> { f | repo = url })
            in
            ( { model | form = newForm }, Cmd.none )

        RepoUrlSubmitted ->
            ( model
            , Nav.pushUrl model.nav ("/repo/" ++ model.form.repo)
            )

        StartChanged t ->
            let
                newForm =
                    model.form
                        |> (\f -> { f | start = t })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl model.route)
            )

        EndChanged t ->
            let
                newForm =
                    model.form
                        |> (\f -> { f | end = t })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl model.route)
            )

        TagAdded tag ->
            let
                newForm =
                    model.form
                        |> (\f -> { f | tags = Set.insert tag f.tags })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl newForm)
            )

        TagExcluded tag ->
            let
                newForm =
                    model.form
                        |> (\f -> { f | tags = Set.insert ("-" ++ tag) f.tags })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl newForm)
            )

        TagRemoved tag ->
            let
                newForm =
                    model.form
                        |> (\f -> { f | tags = Set.remove tag f.tags })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl newForm)
            )

        ReportRequested ->
            case model.repo of
                Nothing ->
                    ( model, Cmd.none )

                Just repo ->
                    let
                        newRepo =
                            { repo | report = Just { summary = "", suggestions = [], events = [] } }
                    in
                    ( { model | repo = Just newRepo }
                    , Cmd.batch
                        [-- TODO: clusters 10 |> Random.generate ReportTagClustered
                         -- TODO: clusters 100 |> Random.generate ReportEventClustered
                         -- TODO: Claude.summarize model.repo
                        ]
                    )

        ClaudeModelChanged mod ->
            let
                claude =
                    model.claude
                        |> (\c -> { c | model = mod })
            in
            ( { model | claude = claude }, Cmd.none )

        ClaudeAuthChanged auth ->
            let
                claude =
                    model.claude
                        |> (\c -> { c | auth = auth })
            in
            ( { model | claude = claude }, Cmd.none )

        Hovered tags ->
            ( { model | hover = tags }, Cmd.none )

        RepoLoaded value ->
            case D.decodeValue repoDecoder value of
                Ok repo ->
                    ( { model | repo = Just repo }
                    , Cmd.batch
                        [-- TODO: fetchGithubEvents repo
                         -- TODO: fetchGithubUsers repo
                         -- TODO: fetchGithubIssues repo
                        ]
                    )

                Err err ->
                    ( addError ("Failed to decode repo: " ++ D.errorToString err) model
                    , Cmd.none
                    )

        GithubEventsFetched result ->
            case result of
                Ok events ->
                    -- TODO: Update repo.github.events
                    ( model, Cmd.none )

                Err err ->
                    ( addError ("Failed to fetch GitHub events: " ++ httpErrorToString err) model
                    , Cmd.none
                    )

        GithubUsersFetched result ->
            case result of
                Ok users ->
                    -- TODO: Update repo.github.users
                    ( model, Cmd.none )

                Err err ->
                    ( addError ("Failed to fetch GitHub users: " ++ httpErrorToString err) model
                    , Cmd.none
                    )

        GithubIssuesFetched result ->
            case result of
                Ok issues ->
                    -- TODO: Update repo.github.issues
                    ( model, Cmd.none )

                Err err ->
                    ( addError ("Failed to fetch GitHub issues: " ++ httpErrorToString err) model
                    , Cmd.none
                    )

        JobTick time ->
            -- TODO: Start next job if none are processing
            ( model, Cmd.none )

        JobCompleted index result ->
            -- TODO: Update job at index with result
            ( model, Cmd.none )

        AddError message ->
            ( addError message model, Cmd.none )


addError : String -> Model -> Model
addError message model =
    { model | errors = { message = message, timestamp = 0 } :: model.errors }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


buildUrl : Filters -> String
buildUrl filters =
    let
        base =
            if String.isEmpty filters.repo then
                "/"

            else
                "/repo/" ++ filters.repo

        params =
            [ if String.isEmpty filters.start then
                Nothing

              else
                Just ("start=" ++ filters.start)
            , if String.isEmpty filters.end then
                Nothing

              else
                Just ("end=" ++ filters.end)
            , if Set.isEmpty filters.tags then
                Nothing

              else
                Just ("tags=" ++ String.join "," (Set.toList filters.tags))
            ]
                |> List.filterMap identity
                |> String.join "&"
    in
    if String.isEmpty params then
        base

    else
        base ++ "?" ++ params



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
