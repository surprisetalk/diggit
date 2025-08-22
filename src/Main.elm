port module Main exposing (..)

---- IMPORTS ------------------------------------------------------------------

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H exposing (Html, text)
import Html.Attributes as A
import Html.Events as A
import Html.Lazy as H
import Html.Style as S
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Markdown
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


formatEventDate : Time.Zone -> Time.Posix -> String
formatEventDate timezone posix =
    let
        -- posix is already Time.Posix, no conversion needed
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



---- PORTS --------------------------------------------------------------------


port requestRepo : String -> Cmd msg


port repoLoaded : (D.Value -> msg) -> Sub msg


port pageErrored : (String -> msg) -> Sub msg


port progressReported : ({ message : String, progress : Float } -> msg) -> Sub msg


port saveToLocalStorage : { key : String, value : String } -> Cmd msg


port saveGithubData : { repo : String, data : E.Value } -> Cmd msg


port githubDataChanged : (E.Value -> msg) -> Sub msg



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
    , start : Time.Posix
    , end : Maybe Time.Posix
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


type alias Error =
    { message : String
    , timestamp : Time.Posix
    }


type alias ApiPreviewOptions =
    { date : Bool
    , time : Bool
    , ext : Bool
    , dir : Bool
    , branch : Bool
    , author : Bool
    , title : Bool
    , description : Bool
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
    , timezone : Time.Zone
    , apiPreviewOptions : ApiPreviewOptions
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
                    |> List.sortBy (.start >> Time.posixToMillis >> negate)
            )
        >> Maybe.withDefault []



-- TODO: clusters n = allEvents model |> Random.List.shuffle |> Random.map (KMeans.clusterBy eventVector n)


eventVector : List Tag -> Event -> List Float
eventVector allTags event =
    let
        startMillis =
            Time.posixToMillis event.start |> toFloat

        endMillis =
            event.end
                |> Maybe.map (Time.posixToMillis >> toFloat)
                |> Maybe.withDefault startMillis

        duration =
            case event.end of
                Nothing ->
                    0

                Just endTime ->
                    toFloat (Time.posixToMillis endTime - Time.posixToMillis event.start)
    in
    [ startMillis
    , endMillis
    , duration
    , toFloat event.insertions
    , toFloat event.deletions
    ]
        ++ List.map (\tag -> iif (Set.member tag event.tags) 1.0 0.0) allTags



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
        (D.field "start" (D.float |> D.map (round >> Time.millisToPosix)))
        (D.maybe (D.field "end" (D.float |> D.map (round >> Time.millisToPosix))))
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
        (D.maybe
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
            |> D.map (Maybe.withDefault Dict.empty)
        )
        (D.maybe (D.field "events" (D.dict eventDecoder)) |> D.map (Maybe.withDefault Dict.empty))
        (D.maybe (D.field "users" (D.dict githubUserDecoder)) |> D.map (Maybe.withDefault Dict.empty))


githubUserDecoder : D.Decoder GithubUser
githubUserDecoder =
    D.map5 GithubUser
        (D.field "id" (D.int |> D.map String.fromInt))
        (D.field "login" D.string)
        (D.maybe (D.field "name" D.string))
        (D.field "avatar_url" D.string)
        (D.field "html_url" D.string)


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


claudeResponseDecoder : D.Decoder String
claudeResponseDecoder =
    D.field "content"
        (D.index 0
            (D.field "text" D.string)
        )


githubEventsDecoder : D.Decoder (List Event)
githubEventsDecoder =
    D.list
        (D.map8
            (\id url createdAt actor eventType payload repo commits ->
                { id = id
                , url = url
                , start = createdAt
                , end = Nothing
                , insertions = 0
                , deletions = 0
                , tags = Set.fromList [ "github", eventType, "@" ++ actor ]
                , summary = formatGithubEventSummary eventType payload
                }
            )
            (D.field "id" D.string)
            (D.oneOf [ D.field "url" D.string, D.succeed "" ])
            (D.field "created_at" Iso8601.decoder)
            (D.field "actor" (D.field "login" D.string))
            (D.field "type" D.string)
            (D.field "payload" D.value)
            (D.maybe (D.field "repo" (D.field "name" D.string)))
            (D.maybe (D.field "payload" (D.field "commits" (D.list (D.field "sha" D.string)))))
        )


githubIssuesDecoder : D.Decoder (Dict Int Event)
githubIssuesDecoder =
    D.list
        (D.map8
            (\number title url createdAt updatedAt closedAt user isPr ->
                ( number
                , { id = "issue-" ++ String.fromInt number
                  , url = url
                  , start = createdAt
                  , end = closedAt
                  , insertions = 0
                  , deletions = 0
                  , tags =
                        Set.fromList
                            [ "github"
                            , iif isPr "pr" "issue"
                            , "@" ++ user
                            , iif (closedAt /= Nothing) "closed" "open"
                            ]
                  , summary = title
                  }
                )
            )
            (D.field "number" D.int)
            (D.field "title" D.string)
            (D.field "html_url" D.string)
            (D.field "created_at" Iso8601.decoder)
            (D.field "updated_at" Iso8601.decoder)
            (D.maybe (D.field "closed_at" Iso8601.decoder))
            (D.field "user" (D.field "login" D.string))
            (D.maybe (D.field "pull_request" D.value) |> D.map ((/=) Nothing))
        )
        |> D.map Dict.fromList


formatGithubEventSummary : String -> D.Value -> String
formatGithubEventSummary eventType payload =
    case eventType of
        "PushEvent" ->
            "Pushed commits"

        "CreateEvent" ->
            "Created branch or tag"

        "DeleteEvent" ->
            "Deleted branch or tag"

        "IssuesEvent" ->
            "Issue activity"

        "PullRequestEvent" ->
            "Pull request activity"

        "WatchEvent" ->
            "Starred repository"

        "ForkEvent" ->
            "Forked repository"

        _ ->
            eventType


encodeEvent : Event -> E.Value
encodeEvent event =
    E.object
        [ ( "id", E.string event.id )
        , ( "url", E.string event.url )
        , ( "start", E.float (Time.posixToMillis event.start |> toFloat) )
        , ( "end"
          , case event.end of
                Just endTime ->
                    E.float (Time.posixToMillis endTime |> toFloat)

                Nothing ->
                    E.null
          )
        , ( "insertions", E.int event.insertions )
        , ( "deletions", E.int event.deletions )
        , ( "tags", E.list E.string (Set.toList event.tags) )
        , ( "summary", E.string event.summary )
        ]


encodeGithubUser : GithubUser -> E.Value
encodeGithubUser user =
    E.object
        [ ( "id", E.string user.id )
        , ( "login", E.string user.login )
        , ( "name"
          , case user.name of
                Just name ->
                    E.string name

                Nothing ->
                    E.null
          )
        , ( "avatar_url", E.string user.avatarUrl )
        , ( "html_url", E.string user.htmlUrl )
        ]


githubDataDecoder : D.Decoder { repo : String, data : Github }
githubDataDecoder =
    D.map2 (\repo data -> { repo = repo, data = data })
        (D.field "repo" D.string)
        (D.field "data" githubDecoder)



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
            , repos = [ "surprisetalk/diggit", "sindresorhus/awesome", "automerge/automerge", "PRQL/prql", "charmbracelet/lipgloss", "munificent/vigil", "surprisetalk/blogs.hn", "tekknolagi/scrapscript" ]
            , hover = Set.empty
            , form = filters
            , route = filters
            , repo = Nothing
            , claude =
                { auth = Maybe.withDefault "" flags.claudeAuth
                , model = Maybe.withDefault "sonnet41" flags.claudeModel
                , history = []
                }
            , timezone = timezone
            , apiPreviewOptions =
                { date = True
                , time = False
                , ext = True
                , dir = False
                , branch = False
                , author = True
                , title = True
                , description = False
                }
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
    | ClaudeResponseReceived (Result Http.Error String)
    | Hovered (Set Tag)
    | RepoLoaded D.Value
    | GithubEventsFetched Int (Maybe Time.Posix) (Result Http.Error (List Event))
    | GithubIssuesFetched Int (Maybe Time.Posix) (Result Http.Error (Dict Int Event))
    | GithubUsersFetched (Result Http.Error (List GithubUser))
    | AddError String
    | PageErrored String
    | ProgressReported { message : String, progress : Float }
    | RemoveError Int
    | GithubDataChanged E.Value
    | ApiPreviewToggled String Bool



---- SUBSCRIPTIONS ------------------------------------------------------------


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ repoLoaded RepoLoaded
        , pageErrored PageErrored
        , progressReported ProgressReported
        , githubDataChanged GithubDataChanged
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
            , Nav.pushUrl model.nav (buildUrl { form | start = t })
            )

        EndChanged t ->
            ( { model | form = { form | end = t } }
            , Nav.pushUrl model.nav (buildUrl { form | end = t })
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
                    let
                        eventsText =
                            formatEventsForApi model.timezone model.apiPreviewOptions (allEvents model)

                        fullPrompt =
                            eventsText ++ "\n\n" ++ summarizerPrompt

                        httpRequest =
                            Http.request
                                { method = "POST"
                                , headers =
                                    [ Http.header "x-api-key" model.claude.auth
                                    , Http.header "anthropic-version" "2023-06-01"
                                    , Http.header "content-type" "application/json"
                                    , Http.header "anthropic-dangerous-direct-browser-access" "true"
                                    ]
                                , url = "https://api.anthropic.com/v1/messages"
                                , body = Http.jsonBody (encodeClaudeRequest model.claude.model fullPrompt)
                                , expect = Http.expectString ClaudeResponseReceived
                                , timeout = Just 60000
                                , tracker = Nothing
                                }
                    in
                    ( { model | repo = Just { repo | report = Just { summary = "", suggestions = [], events = [] } } }
                    , httpRequest
                    )

        ClaudeModelChanged model_ ->
            ( { model | claude = { claude | model = model_ } }
            , saveToLocalStorage { key = "claudeModel", value = model_ }
            )

        ClaudeAuthChanged auth ->
            ( { model | claude = { claude | auth = auth } }
            , saveToLocalStorage { key = "claudeAuth", value = auth }
            )

        ClaudeResponseReceived result ->
            case ( result, model.repo ) of
                ( Ok response, Just repo ) ->
                    case D.decodeString claudeResponseDecoder response of
                        Ok decodedResponse ->
                            ( { model | repo = Just { repo | report = Just { summary = decodedResponse, suggestions = [], events = [] } } }
                            , Cmd.none
                            )

                        Err _ ->
                            ( addError "Failed to decode Claude response" model, Cmd.none )

                ( Err httpError, _ ) ->
                    ( addError ("Claude API error: " ++ httpErrorToString httpError) model, Cmd.none )

                ( _, Nothing ) ->
                    ( model, Cmd.none )

        Hovered tags ->
            ( { model | hover = tags }, Cmd.none )

        RepoLoaded value ->
            case D.decodeValue repoDecoder value of
                Ok repo ->
                    let
                        latestEventTime =
                            repo.github.events
                                |> Dict.values
                                |> List.map (.start >> Time.posixToMillis)
                                |> List.maximum
                                |> Maybe.map Time.millisToPosix

                        latestIssueTime =
                            repo.github.issues
                                |> Dict.values
                                |> List.map (.start >> Time.posixToMillis)
                                |> List.maximum
                                |> Maybe.map Time.millisToPosix
                    in
                    ( { model | repo = Just repo }
                    , Cmd.batch
                        [ Cmd.none -- TODO: fetchGithubEvents 1 latestEventTime repo.url
                        , fetchGithubIssues 1 latestIssueTime repo.url
                        ]
                    )

                Err err ->
                    ( addError ("Failed to decode repo: " ++ D.errorToString err) model
                    , Cmd.none
                    )

        GithubEventsFetched page maybeSince (Ok events) ->
            case model.repo of
                Just repo ->
                    let
                        newEvents =
                            events |> List.map (\event -> ( event.id, event )) |> Dict.fromList

                        githubData =
                            E.object
                                [ ( "events", E.dict identity encodeEvent newEvents )
                                ]
                    in
                    ( model
                    , Cmd.batch
                        [ saveGithubData { repo = repo.url, data = githubData }
                        , collectGithubUsers events |> fetchGithubUsers
                        , if List.length events > 0 then
                            fetchGithubEvents (page + 1) maybeSince repo.url

                          else
                            Cmd.none
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        GithubEventsFetched _ _ (Err err) ->
            ( addError ("Failed to fetch GitHub events: " ++ httpErrorToString err) model, Cmd.none )

        GithubIssuesFetched page maybeSince (Ok issues) ->
            case model.repo of
                Just repo ->
                    let
                        githubData =
                            E.object
                                [ ( "issues", E.dict String.fromInt encodeEvent issues )
                                ]
                    in
                    ( model
                    , Cmd.batch
                        [ saveGithubData { repo = repo.url, data = githubData }
                        , if Dict.size issues > 0 then
                            fetchGithubIssues (page + 1) maybeSince repo.url

                          else
                            Cmd.none
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        GithubIssuesFetched _ _ (Err err) ->
            ( addError ("Failed to fetch GitHub issues: " ++ httpErrorToString err) model, Cmd.none )

        GithubUsersFetched (Ok users) ->
            case model.repo of
                Just repo ->
                    let
                        newUsers =
                            users |> List.map (\user -> ( user.id, user )) |> Dict.fromList

                        githubData =
                            E.object
                                [ ( "users", E.dict identity encodeGithubUser newUsers )
                                ]
                    in
                    ( model, saveGithubData { repo = repo.url, data = githubData } )

                Nothing ->
                    ( model, Cmd.none )

        GithubUsersFetched (Err err) ->
            ( addError ("Failed to fetch GitHub users: " ++ httpErrorToString err) model, Cmd.none )

        AddError message ->
            ( addError message model, Cmd.none )

        PageErrored message ->
            ( addError message model, Cmd.none )

        ProgressReported { message, progress } ->
            ( { model | progress = Dict.insert message progress model.progress }, Cmd.none )

        RemoveError index ->
            ( { model | errors = List.indexedMap (\i e -> iif (i == index) Nothing (Just e)) model.errors |> List.filterMap identity }, Cmd.none )

        GithubDataChanged value ->
            case D.decodeValue githubDataDecoder value of
                Ok { repo, data } ->
                    case model.repo of
                        Just currentRepo ->
                            if currentRepo.url == repo then
                                ( { model | repo = Just { currentRepo | github = data } }, Cmd.none )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ApiPreviewToggled field value ->
            let
                opts =
                    model.apiPreviewOptions

                newOpts =
                    case field of
                        "date" ->
                            { opts | date = value }

                        "time" ->
                            { opts | time = value }

                        "ext" ->
                            { opts | ext = value }

                        "dir" ->
                            { opts | dir = value }

                        "branch" ->
                            { opts | branch = value }

                        "author" ->
                            { opts | author = value }

                        "title" ->
                            { opts | title = value }

                        "description" ->
                            { opts | description = value }

                        _ ->
                            opts
            in
            ( { model | apiPreviewOptions = newOpts }, Cmd.none )


addError : String -> Model -> Model
addError message model =
    { model | errors = { message = message, timestamp = Time.millisToPosix 0 } :: model.errors }


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


encodeClaudeRequest : String -> String -> E.Value
encodeClaudeRequest model prompt =
    E.object
        [ ( "model", E.string (mapClaudeModel model) )
        , ( "max_tokens", E.int 4096 )
        , ( "messages"
          , E.list identity
                [ E.object
                    [ ( "role", E.string "user" )
                    , ( "content", E.string prompt )
                    ]
                ]
          )
        ]


mapClaudeModel : String -> String
mapClaudeModel model =
    -- TODO: Inline these model names into the dropdown.
    case model of
        "opus41" ->
            "claude-3-opus-20240229"

        "sonnet41" ->
            "claude-3-5-sonnet-20241022"

        "haiku35" ->
            "claude-3-5-haiku-20241022"

        _ ->
            "claude-3-5-sonnet-20241022"


extractRepoFromUrl : String -> Maybe ( String, String )
extractRepoFromUrl url =
    url
        |> String.replace "https://github.com/" ""
        |> String.split "/"
        |> (\parts ->
                case parts of
                    [ owner, repo ] ->
                        Just ( owner, repo )

                    _ ->
                        Nothing
           )


fetchGithubEvents : Int -> Maybe Time.Posix -> String -> Cmd Msg
fetchGithubEvents page maybeSince repoUrl =
    case extractRepoFromUrl repoUrl of
        Just ( owner, repoName ) ->
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Accept" "application/vnd.github.v3+json"
                    , Http.header "User-Agent" "diggit-app"
                    ]
                , url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repoName ++ "/events?per_page=100&page=" ++ String.fromInt page
                , body = Http.emptyBody
                , expect = Http.expectJson (GithubEventsFetched page maybeSince) githubEventsDecoder
                , timeout = Just 30000
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


fetchGithubIssues : Int -> Maybe Time.Posix -> String -> Cmd Msg
fetchGithubIssues page maybeSince repoUrl =
    case extractRepoFromUrl repoUrl of
        Just ( owner, repoName ) ->
            let
                sinceParam =
                    case maybeSince of
                        Just posix ->
                            "&since=" ++ Iso8601.fromTime posix

                        Nothing ->
                            ""

                baseUrl =
                    "https://api.github.com/repos/" ++ owner ++ "/" ++ repoName ++ "/issues?state=all&per_page=100&direction=asc&page=" ++ String.fromInt page

                urlWithSince =
                    baseUrl ++ sinceParam
            in
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Accept" "application/vnd.github.v3+json"
                    , Http.header "User-Agent" "diggit-app"
                    ]
                , url = urlWithSince
                , body = Http.emptyBody
                , expect = Http.expectJson (GithubIssuesFetched page maybeSince) githubIssuesDecoder
                , timeout = Just 30000
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


fetchGithubUsers : List String -> Cmd Msg
fetchGithubUsers userLogins =
    case userLogins of
        [] ->
            Cmd.none

        login :: _ ->
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Accept" "application/vnd.github.v3+json"
                    , Http.header "User-Agent" "diggit-app"
                    ]
                , url =
                    "https://api.github.com/users/" ++ login
                , body = Http.emptyBody
                , expect = Http.expectJson GithubUsersFetched (githubUserDecoder |> D.map List.singleton)
                , timeout = Just 30000
                , tracker = Nothing
                }


collectGithubUsers : List Event -> List String
collectGithubUsers events =
    events
        |> List.concatMap (.tags >> Set.toList)
        |> List.filterMap
            (\tag ->
                if String.startsWith "@" tag then
                    Just (String.dropLeft 1 tag)

                else
                    Nothing
            )
        |> List.take 10



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


viewHistogram : Time.Zone -> List Event -> Html Msg
viewHistogram timezone events =
    if List.isEmpty events then
        H.div [] []

    else
        -- TODO: Selecting/dragging should set start/end.
        -- TODO: Hovering should have some effect to indicate that selecting/dragging is possible.
        H.div [ A.class "histogram-container", S.heightPx 120, S.marginTopPx 20, S.marginBottomPx 10 ]
            [ C.chart
                [ CA.height 120
                , CA.width 400
                , CA.padding { top = 0, bottom = 0, left = 0, right = 0 }
                ]
                [ C.bars
                    [ CA.x1 (Tuple.first >> toFloat)
                    ]
                    [ C.bar (Tuple.second >> logBase 10) [ CA.color "#0969da", CA.opacity 0.8 ]
                    ]
                  <|
                    Dict.toList <|
                        List.foldl (\event -> Dict.update (Time.posixToMillis event.start // (30 * 24 * 60 * 60 * 1000) * 30 * 24 * 60 * 60 * 1000) (Maybe.withDefault 1 >> (+) 1 >> Just)) Dict.empty <|
                            events
                , C.xLabels
                    [ CA.noGrid
                    , CA.amount 4
                    , CA.times timezone
                    ]
                ]
            ]


viewFiltersSection : Model -> List Event -> Html Msg
viewFiltersSection model filteredEvents =
    H.section []
        [ H.div [ A.class "filter-count" ]
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
        , viewHistogram model.timezone filteredEvents
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


viewApiCheckbox : String -> String -> Bool -> Html Msg
viewApiCheckbox field label checked =
    H.label [ S.display "flex", S.alignItems "center", S.gap "5px", S.cursor "pointer" ]
        [ H.input
            [ A.type_ "checkbox"
            , A.checked checked
            , A.onCheck (ApiPreviewToggled field)
            ]
            []
        , H.span [] [ text label ]
        ]


summarizerPrompt : String
summarizerPrompt =
    -- TODO: Add repo name, e.g. github.com/owner/repo
    "Please provide a comprehensive summary of this repository's development activity in markdown format with links where possible.\nInclude key insights about development patterns, major contributors, and notable changes.\nUse markdown features like links to commits/issues/PRs, headings, lists, and code blocks to make the output well-structured and navigable."


viewClaudeAside : Model -> List Event -> Html Msg
viewClaudeAside model filteredEvents =
    let
        reportStatus =
            model.repo
                |> Maybe.andThen .report

        apiPreviewContent =
            case reportStatus of
                Nothing ->
                    H.pre [ A.class "api-preview" ]
                        [ text (formatEventsForApi model.timezone model.apiPreviewOptions filteredEvents ++ "\n\n" ++ summarizerPrompt) ]

                Just report ->
                    if String.isEmpty report.summary then
                        H.div [ A.class "api-preview" ]
                            [ H.p [ S.padding "20px", S.textAlign "center" ]
                                [ text "Generating summary..." ]
                            ]

                    else
                        H.div [ A.class "api-preview", S.padding "20px" ]
                            [ Markdown.toHtml [] report.summary ]
    in
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
                    , A.disabled (reportStatus /= Nothing)
                    ]
                    [ text "Generate Report" ]
                ]
            ]
        , H.section []
            [ H.h3 [] [ text "API Preview" ]
            , H.div [ A.class "api-preview-options", S.padding "10px", S.display "flex", S.flexWrap "wrap", S.gap "10px" ]
                [ viewApiCheckbox "date" "Date" model.apiPreviewOptions.date
                , viewApiCheckbox "time" "Time" model.apiPreviewOptions.time
                , viewApiCheckbox "ext" "Ext" model.apiPreviewOptions.ext
                , viewApiCheckbox "dir" "Dir" model.apiPreviewOptions.dir
                , viewApiCheckbox "branch" "Branch" model.apiPreviewOptions.branch
                , viewApiCheckbox "author" "Author" model.apiPreviewOptions.author
                , viewApiCheckbox "title" "Title" model.apiPreviewOptions.title
                , viewApiCheckbox "description" "Description" model.apiPreviewOptions.description
                ]
            , apiPreviewContent
            ]
        ]


formatEventsForApi : Time.Zone -> ApiPreviewOptions -> List Event -> String
formatEventsForApi timezone options events =
    events
        |> List.map
            (\event ->
                let
                    dateTime =
                        formatEventDate timezone event.start

                    datePart =
                        String.left 10 dateTime

                    timePart =
                        String.dropLeft 11 dateTime

                    parts =
                        List.filterMap identity
                            [ if options.date then
                                Just datePart

                              else
                                Nothing
                            , if options.time then
                                Just timePart

                              else
                                Nothing
                            , if options.ext then
                                event.tags
                                    |> Set.toList
                                    |> List.filter (String.startsWith ".")
                                    |> String.join " "
                                    |> (\s ->
                                            if String.isEmpty s then
                                                Nothing

                                            else
                                                Just s
                                       )

                              else
                                Nothing
                            , if options.dir then
                                event.tags
                                    |> Set.toList
                                    |> List.filter (String.startsWith "/")
                                    |> String.join " "
                                    |> (\s ->
                                            if String.isEmpty s then
                                                Nothing

                                            else
                                                Just s
                                       )

                              else
                                Nothing
                            , if options.branch then
                                event.tags
                                    |> Set.toList
                                    |> List.filter (String.startsWith ">")
                                    |> String.join " "
                                    |> (\s ->
                                            if String.isEmpty s then
                                                Nothing

                                            else
                                                Just s
                                       )

                              else
                                Nothing
                            , if options.author then
                                event.tags
                                    |> Set.toList
                                    |> List.filter (String.startsWith "@")
                                    |> String.join " "
                                    |> (\s ->
                                            if String.isEmpty s then
                                                Nothing

                                            else
                                                Just s
                                       )

                              else
                                Nothing
                            , if options.title then
                                event.summary |> String.split "\n" |> List.head

                              else
                                Nothing
                            , if options.description then
                                event.summary |> String.split "\n" |> List.tail |> Maybe.map (String.join " ")

                              else
                                Nothing
                            ]
                in
                String.join " " parts
            )
        |> String.join "\n"


viewMain : Model -> List Event -> Html Msg
viewMain model filteredEvents =
    H.main_ [ A.class "main-content" ]
        [ viewProgressBars model
        , case model.repo of
            Nothing ->
                viewEmptyState

            Just _ ->
                H.div []
                    [ viewEventsSection filteredEvents model
                    , viewVisualization filteredEvents
                    ]
        ]


viewEmptyState : Html Msg
viewEmptyState =
    H.div [ A.class "empty-state" ]
        [ H.h2 [] [ text "Welcome to Diggit" ]
        , H.p [] [ text "Select a repository to start exploring its architecture" ]
        ]


viewSuggestion : Suggestion -> Html Msg
viewSuggestion suggestion =
    H.a [ A.href "#", A.class "suggestion" ]
        [ text suggestion.text ]


viewEventsSection : List Event -> Model -> Html Msg
viewEventsSection events model =
    H.div [ A.class "events-list" ]
        (events
            |> List.take 1000
            -- Limit to first 1000 events for performance
            |> List.map (viewEvent model)
        )



-- TODO: If showing exactly 1000, then display a little message that says that 1000 events is maximum.


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
