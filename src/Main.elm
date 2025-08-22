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
import Time
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


formatEventDate : Time.Zone -> Float -> String
formatEventDate timezone time =
    let
        posix =
            time
                |> round
                |> Time.millisToPosix

        date =
            Date.fromPosix timezone posix

        hour =
            Time.toHour timezone posix
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute timezone posix
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    Date.format "yyyy-MM-dd" date ++ " " ++ hour ++ ":" ++ minute


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
    , query : String
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


type alias ClaudeModel =
    String


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
    , timezone : Time.Zone
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
    , timezone : Maybe Int
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url nav =
    let
        filters =
            router url

        timezone =
            flags.timezone
                |> Maybe.map (\offset -> Time.customZone offset [])
                |> Maybe.withDefault Time.utc

        model =
            { nav = nav
            , errors = []
            , progress = Dict.empty
            , repos = [ "surprisetalk/diggit", "sindresorhus/awesome", "automerge/automerge", "PRQL/prql", "charmbracelet/lipgloss", "munificent/vigil", "surprisetalk/blogs.hn" ]
            , hover = Set.empty
            , form = filters
            , route = filters
            , repo = Nothing
            , claude =
                { auth = Maybe.withDefault "" flags.claudeAuth
                , model = Maybe.withDefault "sonnet41" flags.claudeModel
                , history = []
                }
            , jobs = Array.empty
            , timezone = timezone
            }
    in
    ( model, requestRepo filters.repo )


defaultFilters : Filters
defaultFilters =
    { repo = "", start = "", end = "", tags = Set.empty, query = "" }


router : Url -> Filters
router =
    let
        -- e.g. /repo/ziglang/zig?start=20240401&end=20250401&tags=\>main,@sally&q=TODO#202404
        routeParser : UrlP.Parser (Filters -> a) a
        routeParser =
            (UrlP.top
                </> UrlP.string
                </> UrlP.string
                <?> UrlQ.string "start"
                <?> UrlQ.string "end"
                <?> UrlQ.string "tags"
                <?> UrlQ.string "q"
            )
                |> UrlP.map
                    (\owner repo maybeStart maybeEnd maybeTags maybeQuery ->
                        { repo = owner ++ "/" ++ repo
                        , start = Maybe.withDefault "" maybeStart
                        , end = Maybe.withDefault "" maybeEnd
                        , tags =
                            maybeTags
                                |> Maybe.withDefault ""
                                |> String.split ","
                                |> List.filter (not << String.isEmpty)
                                |> Set.fromList
                        , query = Maybe.withDefault "" maybeQuery
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
    | QueryChanged String
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
            ( { model | route = router url, form = { form | repo = (router url).repo }, repo = iif (model.route.repo == (router url).repo) model.repo Nothing }
            , iif (model.route.repo == (router url).repo) Cmd.none (requestRepo (router url).repo)
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
            ( { model | form = { form | start = t } }
            , Nav.pushUrl model.nav (buildUrl model.route)
            )

        EndChanged t ->
            ( { model | form = { form | end = t } }
            , Nav.pushUrl model.nav (buildUrl model.route)
            )

        QueryChanged q ->
            ( { model | form = { form | query = q } }
            , Nav.pushUrl model.nav (buildUrl { form | query = q })
            )

        TagAdded tag ->
            ( { model | form = { form | tags = Set.insert tag form.tags } }
            , Nav.pushUrl model.nav (buildUrl { form | tags = Set.insert tag form.tags })
            )

        TagExcluded tag ->
            ( { model | form = { form | tags = Set.insert ("-" ++ tag) form.tags } }
            , Nav.pushUrl model.nav (buildUrl { form | tags = Set.insert ("-" ++ tag) form.tags })
            )

        TagRemoved tag ->
            ( { model | form = { form | tags = Set.remove tag form.tags } }
            , Nav.pushUrl model.nav (buildUrl { form | tags = Set.remove tag form.tags })
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
                        ]
                    )

        ClaudeModelChanged model_ ->
            ( { model | claude = { claude | model = model_ } }, Cmd.none )

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

        GithubEventsFetched (Ok events) ->
            -- TODO: Update repo.github.events
            ( model, Cmd.none )

        GithubEventsFetched (Err err) ->
            ( addError ("Failed to fetch GitHub events: " ++ httpErrorToString err) model, Cmd.none )

        GithubUsersFetched (Ok users) ->
            -- TODO: Update repo.github.users
            ( model, Cmd.none )

        GithubUsersFetched (Err err) ->
            ( addError ("Failed to fetch GitHub users: " ++ httpErrorToString err) model, Cmd.none )

        GithubIssuesFetched (Ok issues) ->
            -- TODO: Update repo.github.issues
            ( model, Cmd.none )

        GithubIssuesFetched (Err err) ->
            ( addError ("Failed to fetch GitHub issues: " ++ httpErrorToString err) model, Cmd.none )

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
            ( { model | errors = List.indexedMap (\i e -> iif (i == index) Nothing (Just e)) model.errors |> List.filterMap identity }, Cmd.none )


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
    -- TODO: Use Url.Builder.
    let
        base =
            "/" ++ iif (String.isEmpty filters.repo) "" filters.repo

        params =
            [ ( "start", filters.start ), ( "end", filters.end ), ( "tags", String.join "," (Set.toList filters.tags) ), ( "q", filters.query ) ]
                |> List.filter (\( _, v ) -> not (String.isEmpty v || (v == "")))
                |> List.map (\( k, v ) -> k ++ "=" ++ v)
                |> String.join "&"
    in
    base ++ iif (String.isEmpty params) "" ("?" ++ params)



---- VIEW ---------------------------------------------------------------------


view : Model -> Browser.Document Msg
view model =
    let
        filteredEvents =
            allEvents model
                |> List.filter
                    (\event ->
                        let
                            eventStartDate =
                                formatEventDate model.timezone event.start

                            startOk =
                                String.isEmpty model.route.start
                                    || (model.route.start <= eventStartDate)

                            endOk =
                                String.isEmpty model.route.end
                                    || (case event.end of
                                            Nothing ->
                                                True

                                            Just e ->
                                                formatEventDate model.timezone e <= model.route.end
                                       )

                            tagsOk =
                                Set.isEmpty model.route.tags
                                    || Set.isEmpty (Set.diff model.route.tags event.tags)

                            queryOk =
                                String.isEmpty model.route.query
                                    || String.contains (String.toLower model.route.query) (String.toLower event.summary)
                        in
                        startOk && endOk && tagsOk && queryOk
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
            , viewClaudeAside model filteredEvents
            , viewErrors model
            ]
        ]
    }


viewAside : Model -> List Event -> List Tag -> List ( Tag, Int ) -> Html Msg
viewAside model filteredEvents allTags tagFrequencies =
    H.aside [ A.class "sidebar", S.borderRight "1px solid #30363d" ]
        [ viewHeader
        , viewRepoSection model
        , viewFiltersSection model filteredEvents
        , viewTagsSection model tagFrequencies
        ]


viewHeader : Html Msg
viewHeader =
    H.header [ A.class "header" ]
        [ H.h1 [] [ H.a [ A.href "/" ] [ text "DIGGIT.DEV" ] ]
        , H.h2 [] [ text "for architecture archaeologists" ]
        , H.div [ A.class "header-links" ]
            [ H.a [ A.href "https://taylor.town", A.target "_blank" ] [ text "by taylor.town" ]
            , H.a [ A.href "https://github.com/surprisetalk/diggit", A.target "_blank" ] [ text "view on github" ]
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
                    , A.class "input"
                    ]
                    []
                , H.button
                    [ A.type_ "submit"
                    , A.class "primary-btn"
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
                [ A.type_ "text"
                , A.placeholder "Search events..."
                , A.value model.form.query
                , A.onInput QueryChanged
                , A.class "input"
                ]
                []
            ]
        , H.div [ A.class "form-row" ]
            [ H.input
                [ A.type_ "date"
                , A.placeholder "Start date"
                , A.value model.form.start
                , A.onInput StartChanged
                , A.class "small-input"
                ]
                []
            , H.input
                [ A.type_ "date"
                , A.placeholder "End date"
                , A.value model.form.end
                , A.onInput EndChanged
                , A.class "small-input"
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
                                , A.class (iif (String.startsWith "-" tag) "exclude-tag-btn" "active-tag-btn")
                                ]
                                [ text ("× " ++ tag) ]
                        )
                )
            ]
        , iif (List.isEmpty tagFrequencies) (text "") <|
            H.div []
                [ H.div [ A.class "section-title" ] [ text "Popular tags" ]
                , H.div [ A.class "tag-filters" ]
                    (tagFrequencies
                        |> List.filter (\( tag, _ ) -> not (Set.member tag model.form.tags))
                        |> List.take 100
                        |> List.map (\( tag, count ) -> tagButton (tag ++ " (" ++ String.fromInt count ++ ")") (TagAdded tag))
                    )
                ]
        ]


tagButton : String -> Msg -> Html Msg
tagButton label msg =
    H.button [ A.onClick msg, A.class "small-btn" ]
        [ text label ]


viewClaudeAside : Model -> List Event -> Html Msg
viewClaudeAside model filteredEvents =
    H.aside [ A.class "sidebar", S.borderLeft "1px solid #30363d" ]
        [ H.section []
            [ H.h3 [] [ text "Claude Settings" ]
            , H.div [ A.class "claude-form" ]
                [ H.input
                    [ A.type_ "password"
                    , A.placeholder "Anthropic API Key"
                    , A.value model.claude.auth
                    , A.onInput ClaudeAuthChanged
                    , A.class "input"
                    ]
                    []
                , H.select
                    [ A.onInput ClaudeModelChanged
                    , A.class "select"
                    ]
                    [ H.option [ A.value "opus41", A.selected (model.claude.model == "opus41") ] [ text "Opus 4.1" ]
                    , H.option [ A.value "sonnet41", A.selected (model.claude.model == "sonnet41") ] [ text "Sonnet 4.1" ]
                    , H.option [ A.value "haiku35", A.selected (model.claude.model == "haiku35") ] [ text "Haiku 3.5" ]
                    ]
                , H.button
                    [ A.onClick ReportRequested
                    , A.class "primary-btn"
                    , A.type_ "button"
                    , A.disabled (model.repo |> Maybe.andThen .report |> (/=) Nothing)
                    ]
                    [ text "Generate Report" ]
                ]
            ]
        , H.section []
            [ H.h3 [] [ text "API Preview" ]
            , H.pre [ A.class "api-preview" ] [ text (formatEventsForApi model.timezone filteredEvents) ]
            ]
        ]


formatEventsForApi : Time.Zone -> List Event -> String
formatEventsForApi timezone events =
    events
        |> List.map
            (\event ->
                String.join " "
                    [ formatEventDate timezone event.start
                    , Set.toList event.tags |> String.join " "
                    , String.replace "\n" " " event.summary
                    ]
            )
        |> String.join "\n"


viewMain : Model -> List Event -> Html Msg
viewMain model filteredEvents =
    H.main_ [ A.class "main-content" ]
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
    H.div [ A.class "empty-state" ]
        [ H.h2 [] [ text "Welcome to Diggit" ]
        , H.p [] [ text "Select a repository to start exploring its architecture" ]
        ]


viewReportSection : Repo -> Model -> Html Msg
viewReportSection repo model =
    case repo.report of
        Nothing ->
            text ""

        Just report ->
            H.div [ A.class "report-section" ]
                [ H.div [ A.class "report-summary" ]
                    [ H.h3 [] [ text "AI Summary" ]
                    , H.p [] [ text (iif (String.isEmpty report.summary) "Generating summary..." report.summary) ]
                    ]
                , iif (List.isEmpty report.suggestions) (text "") <| H.div [ A.class "suggestions" ] (List.map viewSuggestion report.suggestions)
                ]


viewSuggestion : Suggestion -> Html Msg
viewSuggestion suggestion =
    H.a [ A.href "#", A.class "suggestion" ]
        [ text suggestion.text ]


viewEventsSection : List Event -> Model -> Html Msg
viewEventsSection events model =
    H.div [ A.class "events-section" ]
        [ H.div [ A.class "events-list" ]
            (events
                |> List.take 1000
                -- Limit to first 1000 events for performance
                |> List.map (viewEvent model)
            )

        -- TODO: If showing exactly 1000, then display a little message that says that 1000 events is maximum.
        ]


viewEvent : Model -> Event -> Html Msg
viewEvent model event =
    let
        isHovered =
            not (Set.isEmpty (Set.intersect model.hover event.tags))

        eventDate =
            formatEventDate model.timezone event.start
    in
    H.div
        [ A.class "event-card"
        , S.backgroundColor (iif isHovered "#1c2128" "#161b22")
        , A.onMouseEnter (Hovered event.tags)
        , A.onMouseLeave (Hovered Set.empty)
        ]
        [ H.div [ A.class "event-header" ]
            [ H.a [ A.href event.url, A.target "_blank", A.class "event-link" ]
                [ text (String.left 60 event.summary) ]
            ]
        , H.div [ A.class "event-meta" ]
            [ H.span [] [ text eventDate ]
            , iif (event.insertions > 0 || event.deletions > 0)
                (H.span [ A.class "event-changes" ] [ text ("+" ++ String.fromInt event.insertions ++ " -" ++ String.fromInt event.deletions) ])
                (text "")
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
        , iif (not (String.isEmpty event.summary) && String.length event.summary > 60)
            (H.p [ A.class "event-description" ] [ text event.summary ])
            (text "")
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
    H.div [ A.class "progress-container" ]
        (model.progress
            |> Dict.filter (\_ v -> v < 1)
            |> Dict.toList
            |> List.map viewProgressBar
        )


viewProgressBar : ( String, Float ) -> Html Msg
viewProgressBar ( message, progress ) =
    H.div [ A.class "progress-item" ]
        [ H.div [ A.class "progress-label" ] [ text message ]
        , H.div [ A.class "progress-bar-container" ]
            [ H.div
                [ A.class "progress-bar-fill"
                , S.width (String.fromFloat (progress * 100) ++ "%")
                ]
                []
            ]
        ]


viewErrors : Model -> Html Msg
viewErrors model =
    H.div [ A.class "error-container" ] <|
        List.indexedMap viewError model.errors


viewError : Int -> Error -> Html Msg
viewError index error =
    H.div [ A.class "error-item" ]
        [ H.div [ A.class "error-message" ] [ text error.message ]
        , H.button [ A.onClick (RemoveError index), A.class "error-close" ] [ text "×" ]
        ]
