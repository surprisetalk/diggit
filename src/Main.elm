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
        >> List.indexedMap (\i c -> iif (i > 0 && modBy 3 i == 0) [ ',', c ] [ c ])
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


port pageErrored : (String -> msg) -> Sub msg


port progressReported : ({ message : String, progress : Float } -> msg) -> Sub msg



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


type
    ClaudeModel
    -- TODO: We don't need this. Just use a string.
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
    , progress : Dict String Float
    , repos : List String
    , hover : Set Tag
    , form : Filters
    , route : Filters
    , repo : Maybe Repo
    , claude : Claude
    , jobs : Array Job
    }


allEvents : Model -> List Event
allEvents =
    .repo
        >> Maybe.map
            (\repo ->
                List.concat
                    [ Dict.values repo.commits
                    , Dict.values repo.branches
                    , Dict.values repo.github.issues
                    , Dict.values repo.github.events
                    , repo.report |> Maybe.map .events |> Maybe.withDefault []
                    ]
                    |> List.sortBy (.start >> negate)
            )
        >> Maybe.withDefault []


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
            router url

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
            , progress = Dict.empty
            , repos = [ "surprisetalk/diggit", "sindresorhus/awesome", "automerge/automerge", "PRQL/prql", "charmbracelet/lipgloss", "munificent/vigil", "surprisetalk/blogs.hn" ]
            , hover = Set.empty
            , form = filters
            , route = filters
            , repo = Nothing
            , claude = initialClaude
            , jobs = Array.empty
            }
    in
    ( model
    , requestRepo filters.repo
    )


defaultFilters : Filters
defaultFilters =
    { repo = "", start = "", end = "", tags = Set.empty }


router : Url -> Filters
router =
    let
        -- e.g. /repo/ziglang/zig?start=20240401&end=20250401&tags=\>main,@sally#202404
        routeParser : UrlP.Parser (Filters -> a) a
        routeParser =
            (UrlP.top
                </> UrlP.string
                </> UrlP.string
                <?> UrlQ.string "start"
                <?> UrlQ.string "end"
                <?> UrlQ.string "tags"
            )
                |> UrlP.map
                    (\owner repo maybeStart maybeEnd maybeTags ->
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
                    )
    in
    UrlP.parse routeParser
        >> Maybe.withDefault defaultFilters



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
    | PageErrored String
    | ProgressReported { message : String, progress : Float }
    | RemoveError Int



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ repoLoaded RepoLoaded
        , pageErrored PageErrored
        , progressReported ProgressReported

        -- TODO: Time.every (1000) JobTick
        ]



---- UPDATE -------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form, claude } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            let
                filters =
                    router url
            in
            ( { model | route = filters, form = { form | repo = filters.repo }, repo = iif (model.route.repo == filters.repo) model.repo Nothing }
            , iif (model.route.repo == filters.repo) Cmd.none (requestRepo filters.repo)
            )

        LinkClick (Browser.Internal url) ->
            ( model, Nav.pushUrl model.nav (Url.toString url) )

        LinkClick (Browser.External url) ->
            ( model, Nav.load url )

        RepoUrlChanged url ->
            ( { model | form = { form | repo = url } }, Cmd.none )

        RepoUrlSubmitted ->
            ( { model | form = { defaultFilters | repo = model.form.repo } }
            , Nav.pushUrl model.nav ("/" ++ model.form.repo)
            )

        StartChanged t ->
            -- TODO : Get rid of let statement.
            let
                newForm =
                    model.form
                        |> (\f -> { f | start = t })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl model.route)
            )

        EndChanged t ->
            -- TODO : Get rid of let statement.
            let
                newForm =
                    model.form
                        |> (\f -> { f | end = t })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl model.route)
            )

        TagAdded tag ->
            -- TODO : Get rid of let statement.
            let
                newForm =
                    model.form
                        |> (\f -> { f | tags = Set.insert tag f.tags })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl newForm)
            )

        TagExcluded tag ->
            -- TODO : Get rid of let statement.
            let
                newForm =
                    model.form
                        |> (\f -> { f | tags = Set.insert ("-" ++ tag) f.tags })
            in
            ( { model | form = newForm }
            , Nav.pushUrl model.nav (buildUrl newForm)
            )

        TagRemoved tag ->
            -- TODO : Get rid of let statement.
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
                    ( { model | repo = Just { repo | report = Just { summary = "", suggestions = [], events = [] } } }
                    , Cmd.batch
                        [-- TODO: clusters 10 |> Random.generate ReportTagClustered
                         -- TODO: clusters 100 |> Random.generate ReportEventClustered
                         -- TODO: Claude.summarize model.repo
                        ]
                    )

        ClaudeModelChanged mod ->
            ( { model | claude = { claude | model = mod } }, Cmd.none )

        ClaudeAuthChanged auth ->
            ( { model | claude = { claude | auth = auth } }, Cmd.none )

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
            -- TODO: Use syntax like (GithubEventsFetched (Ok events)).
            case result of
                Ok events ->
                    -- TODO: Update repo.github.events
                    ( model, Cmd.none )

                Err err ->
                    ( addError ("Failed to fetch GitHub events: " ++ httpErrorToString err) model
                    , Cmd.none
                    )

        GithubUsersFetched result ->
            -- TODO: Use syntax like (GithubEventsFetched (Ok events)).
            case result of
                Ok users ->
                    -- TODO: Update repo.github.users
                    ( model, Cmd.none )

                Err err ->
                    ( addError ("Failed to fetch GitHub users: " ++ httpErrorToString err) model
                    , Cmd.none
                    )

        GithubIssuesFetched result ->
            -- TODO: Use syntax like (GithubEventsFetched (Ok events)).
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

        PageErrored message ->
            ( addError message model, Cmd.none )

        ProgressReported { message, progress } ->
            ( { model | progress = Dict.insert message progress model.progress }, Cmd.none )

        RemoveError index ->
            ( { model | errors = List.indexedMap (\i e -> iif (i == index) Nothing (Just e)) model.errors |> List.filterMap identity }
            , Cmd.none
            )


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
    -- TODO: Use Url.Builder instead.
    let
        base =
            "/" ++ iif (String.isEmpty filters.repo) "" filters.repo

        params =
            [ iif (String.isEmpty filters.start) Nothing (Just ("start=" ++ filters.start))
            , iif (String.isEmpty filters.end) Nothing (Just ("end=" ++ filters.end))
            , iif (Set.isEmpty filters.tags) Nothing (Just ("tags=" ++ String.join "," (Set.toList filters.tags)))
            ]
                |> List.filterMap identity
                |> String.join "&"
    in
    base ++ iif (String.isEmpty params) "" ("?" ++ params)



--
---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view model =
    let
        flex =
            H.div << (::) (A.class "flex flex-wrap")

        rows =
            H.div << (::) (A.class "flex flex-col")

        cols attrs =
            H.div << (::) (A.class "flex flex-row")

        filteredEvents =
            allEvents model
                |> List.filter
                    (\event ->
                        let
                            startOk =
                                String.isEmpty model.route.start
                                    || (model.route.start <= String.fromFloat event.start)

                            endOk =
                                String.isEmpty model.route.end
                                    || (case event.end of
                                            Nothing ->
                                                True

                                            Just e ->
                                                String.fromFloat e <= model.route.end
                                       )

                            tagsOk =
                                Set.isEmpty model.route.tags
                                    || not (Set.isEmpty (Set.intersect model.route.tags event.tags))
                        in
                        startOk && endOk && tagsOk
                    )

        allTags =
            allEvents model
                |> List.map .tags
                |> List.foldl Set.union Set.empty
                |> Set.toList

        -- TODO: Calculate tag frequencies for filtered events
        filteredTagFrequencies =
            filteredEvents
                |> List.map .tags
                |> List.concatMap Set.toList
                |> List.foldl
                    (\tag dict ->
                        Dict.update tag
                            (Maybe.withDefault 0 >> (+) 1 >> Just)
                            dict
                    )
                    Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> negate)
    in
    { title = "diggit"
    , body =
        [ H.div [ A.class "app-layout" ]
            [ viewAside model filteredEvents allTags filteredTagFrequencies
            , viewMain model filteredEvents
            , viewErrors model
            ]
        ]
    }


viewAside : Model -> List Event -> List Tag -> List ( Tag, Int ) -> Html Msg
viewAside model filteredEvents allTags tagFrequencies =
    H.aside
        [ A.class "sidebar"
        ]
        [ viewHeader
        , viewRepoSection model
        , viewFiltersSection model filteredEvents
        , viewTagsSection model tagFrequencies
        , viewClaudeSection model
        ]


viewHeader : Html Msg
viewHeader =
    H.header [ A.class "header" ]
        [ H.h1 []
            [ H.a
                [ A.href "/"
                ]
                [ text "DIGGIT.DEV" ]
            ]
        , H.h2 []
            [ text "for architecture archaeologists" ]
        , H.div [ A.class "header-links" ]
            [ H.a
                [ A.href "https://taylor.town"
                , A.target "_blank"
                ]
                [ text "by taylor.town" ]
            , H.a
                [ A.href "https://github.com/surprisetalk/diggit"
                , A.target "_blank"
                ]
                [ text "view on github" ]
            ]
        ]


viewRepoSection : Model -> Html Msg
viewRepoSection model =
    H.section []
        [ H.form [ A.onSubmit RepoUrlSubmitted, A.class "form" ]
            [ H.div [ A.class "form-row" ]
                [ H.input
                    [ A.type_ "text"
                    , A.placeholder "owner/repo"
                    , A.value model.form.repo
                    , A.onInput RepoUrlChanged
                    , A.class "form-input"
                    ]
                    []
                , H.button
                    [ A.type_ "submit"
                    , A.class "btn btn-primary"
                    ]
                    [ text "Load" ]
                ]
            ]
        , H.div [ A.class "repo-list" ]
            (List.map (\repo -> H.a [ A.href ("/" ++ repo) ] [ text repo ])
                model.repos
            )
        ]


viewFiltersSection : Model -> List Event -> Html Msg
viewFiltersSection model filteredEvents =
    H.section []
        [ -- TODO: Add histogram visualization here
          H.div [ A.class "filter-count" ]
            [ H.div [ A.class "filter-info" ]
                [ text ("Showing " ++ String.fromInt (List.length filteredEvents) ++ " events") ]
            ]
        , H.div [ A.class "form-row" ]
            [ H.input
                [ A.type_ "date"
                , A.placeholder "Start date"
                , A.value model.form.start
                , A.onInput StartChanged
                , A.class "form-input-small"
                ]
                []
            , H.input
                [ A.type_ "date"
                , A.placeholder "End date"
                , A.value model.form.end
                , A.onInput EndChanged
                , A.class "form-input-small"
                ]
                []
            ]
        ]


viewTagsSection : Model -> List ( Tag, Int ) -> Html Msg
viewTagsSection model tagFrequencies =
    H.section []
        [ H.div []
            [ H.div [ A.class "section-title" ]
                [ text "Active filters" ]
            , H.div [ A.class "tag-filters" ]
                (Set.toList model.form.tags
                    |> List.map
                        (\tag ->
                            H.button
                                [ A.onClick (TagRemoved tag)
                                , A.class ("btn btn-small " ++ iif (String.startsWith "-" tag) "btn-tag-exclude" "btn-tag-active")
                                ]
                                [ text ("× " ++ tag) ]
                        )
                )
            ]
        , iif (List.isEmpty tagFrequencies) (H.div [] []) <|
            H.div []
                [ H.div [ A.class "section-title" ]
                    [ text "Popular tags" ]
                , H.div [ A.class "tag-filters" ]
                    (tagFrequencies
                        |> List.take 100
                        |> List.map
                            (\( tag, count ) ->
                                tagButton (tag ++ " (" ++ String.fromInt count ++ ")") (TagAdded tag)
                            )
                    )
                ]
        ]


tagButton : String -> Msg -> Html Msg
tagButton label msg =
    H.button
        [ A.onClick msg
        , A.class "btn btn-secondary btn-small"
        ]
        [ text label ]


viewClaudeSection : Model -> Html Msg
viewClaudeSection model =
    let
        totalTokens =
            model.claude.history
                |> List.map .tokens
                |> List.sum

        totalPrice =
            model.claude.history
                |> List.map .price
                |> List.sum
    in
    H.section [ A.class "claude-section section" ]
        [ H.button
            [ A.onClick ReportRequested
            , A.class "btn btn-primary"
            , A.style "padding" "10px 20px"
            , A.style "font-size" "14px"
            , S.marginBottomRem 1
            ]
            [ text "Generate AI Report" ]
        , H.div [ A.class "section-title" ]
            [ text "Claude Settings" ]
        , H.div [ A.class "form-row" ]
            [ H.select
                [ A.onInput
                    (\s ->
                        ClaudeModelChanged
                            (case s of
                                "opus41" ->
                                    Opus41

                                "haiku35" ->
                                    Haiku35

                                _ ->
                                    Sonnet41
                            )
                    )
                , A.class "select"
                ]
                [ H.option [ A.value "opus41", A.selected (model.claude.model == Opus41) ] [ text "Opus 4.1" ]
                , H.option [ A.value "sonnet41", A.selected (model.claude.model == Sonnet41) ] [ text "Sonnet 4.1" ]
                , H.option [ A.value "haiku35", A.selected (model.claude.model == Haiku35) ] [ text "Haiku 3.5" ]
                ]
            ]
        , H.input
            [ A.type_ "password"
            , A.placeholder "API Key"
            , A.value model.claude.auth
            , A.onInput ClaudeAuthChanged
            , A.class "password-input"
            ]
            []
        , H.div [ A.class "claude-stats" ]
            [ H.span [] [ text (commas (String.fromInt totalTokens) ++ " tokens") ]
            , H.span [] [ text (usd totalPrice) ]
            ]
        ]


viewMain : Model -> List Event -> Html Msg
viewMain model filteredEvents =
    H.main_
        [ A.class "main-content"
        ]
        [ viewProgressBars model
        , case model.repo of
            Nothing ->
                viewEmptyState

            Just repo ->
                H.div [ S.displayFlex, S.flexDirectionColumn, S.width "100%" ]
                    [ viewReportSection repo model
                    , viewEventsSection filteredEvents model
                    , viewVisualization filteredEvents
                    ]
        ]


viewEmptyState : Html Msg
viewEmptyState =
    H.div
        [ A.class "empty-state"
        ]
        [ H.h2 []
            [ text "Welcome to Diggit" ]
        , H.p []
            [ text "Select a repository to start exploring its architecture" ]
        ]


viewReportSection : Repo -> Model -> Html Msg
viewReportSection repo model =
    case repo.report of
        Nothing ->
            text ""

        Just report ->
            H.div [ A.class "report-section" ]
                [ H.div [ A.class "report-summary" ]
                    [ H.h3 []
                        [ text "AI Summary" ]
                    , H.p []
                        [ text (iif (String.isEmpty report.summary) "Generating summary..." report.summary)
                        ]
                    ]
                , iif (List.isEmpty report.suggestions) (H.div [] []) <| H.div [ A.class "suggestions" ] (List.map viewSuggestion report.suggestions)
                ]


viewSuggestion : Suggestion -> Html Msg
viewSuggestion suggestion =
    H.a
        [ A.href "#"
        , A.class "suggestion"
        ]
        [ text suggestion.text ]


viewEventsSection : List Event -> Model -> Html Msg
viewEventsSection events model =
    H.div [ A.class "events-section" ]
        [ H.h3 []
            [ text "Events" ]
        , H.div [ A.class "events-list" ]
            (events
                |> List.take 50
                -- Limit to first 50 events for performance
                |> List.map (viewEvent model)
            )
        ]


viewEvent : Model -> Event -> Html Msg
viewEvent model event =
    let
        isHovered =
            not (Set.isEmpty (Set.intersect model.hover event.tags))

        eventDate =
            -- TODO: Properly format timestamp to date string
            String.fromFloat event.start
    in
    H.div
        [ A.class "event-card"
        , A.style "background-color" (iif isHovered "#f8f8f8" "white")
        , A.onMouseEnter (Hovered event.tags)
        , A.onMouseLeave (Hovered Set.empty)
        ]
        [ H.div [ A.class "event-header" ]
            [ H.a
                [ A.href event.url
                , A.target "_blank"
                , A.class "event-link"
                ]
                [ text (String.left 60 event.summary) ]
            ]
        , H.div [ A.class "event-meta" ]
            [ H.span [] [ text eventDate ]
            , iif (not (event.insertions > 0 || event.deletions > 0)) (H.span [] []) <|
                H.span [ A.class "event-changes" ]
                    [ text ("+" ++ String.fromInt event.insertions ++ " -" ++ String.fromInt event.deletions) ]
            , H.div [ A.class "event-tags" ]
                (event.tags
                    |> Set.toList
                    |> List.map
                        (\tag ->
                            H.span
                                [ A.class "tag"
                                ]
                                [ text tag ]
                        )
                )
            ]
        , iif (String.isEmpty event.summary && String.length event.summary > 60) (H.div [] []) <|
            H.p [ A.class "event-description" ]
                [ text event.summary ]
        ]


viewVisualization : List Event -> Html Msg
viewVisualization events =
    -- TODO: Add actual histogram/chart visualization
    -- This would require a charting library or custom SVG implementation
    H.div [ A.class "visualization" ]
        [ H.h3 []
            [ text "Activity Visualization" ]
        , H.p []
            [ text ("Total events: " ++ String.fromInt (List.length events))
            , text " | "
            , text ("Total additions: " ++ String.fromInt (events |> List.map .insertions |> List.sum))
            , text " | "
            , text ("Total deletions: " ++ String.fromInt (events |> List.map .deletions |> List.sum))
            ]

        -- TODO: Add actual histogram chart here
        ]


viewProgressBars : Model -> Html Msg
viewProgressBars model =
    H.div
        [ A.class "progress-container" ]
        (model.progress
            |> Dict.filter (\_ v -> v < 1)
            |> Dict.toList
            |> List.map viewProgressBar
        )


viewProgressBar : ( String, Float ) -> Html Msg
viewProgressBar ( message, progress ) =
    H.div
        [ A.class "progress-item" ]
        [ H.div
            [ A.class "progress-label" ]
            [ text message ]
        , H.div
            [ A.class "progress-bar-container" ]
            [ H.div
                [ A.class "progress-bar-fill"
                , A.style "width" (String.fromFloat (progress * 100) ++ "%")
                ]
                []
            ]
        ]


viewErrors : Model -> Html Msg
viewErrors model =
    H.div
        [ A.class "error-container" ]
        (List.indexedMap viewError model.errors)


viewError : Int -> Error -> Html Msg
viewError index error =
    H.div
        [ A.class "error-item" ]
        [ H.div
            [ A.class "error-message" ]
            [ text error.message ]
        , H.button
            [ A.onClick (RemoveError index)
            , A.class "error-close"
            ]
            [ text "×" ]
        ]
