module Codegen.Simplify exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import FastDict as Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Review.Fix as Fix
import Review.Rule as Rule exposing (ModuleKey, Rule)


{-| Reports when calls to Elm.value can be replaced by more readable alternatives.

    config =
        [ Codegen.Simplify.rule
        ]


## Fail

    todo =
        Elm.value
            { importFrom = [ "Debug" ]
            , name = "todo"
            , annotation = Nothing
            }


## Success

    todo dynamicName =
        Elm.value
            { importFrom = [ "Debug" ]
            , name = dynamicName
            , annotation = Nothing
            }


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template miniBill/elm-review-codegen-simplify/example --rules Codegen.Simplify
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Codegen.Simplify" initialContext
        |> Rule.withModuleVisitor
            (Rule.withExpressionEnterVisitor expressionVisitor
                >> Rule.withImportVisitor importVisitor
                >> Rule.withDeclarationListVisitor declarationListVisitor
            )
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


fromProjectToModule : ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ (Node _ moduleName) _ =
    { isGen = List.head moduleName == Just "Gen"
    , imports = Dict.empty
    , importListEnd = Nothing
    , values = Dict.empty
    , valueCandidates = []
    , callCandidates = []
    }


fromModuleToProject : ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey (Node _ moduleName) context =
    let
        build : ModuleName -> Tree
        build queue =
            case queue of
                [] ->
                    leaf context.values

                head :: tail ->
                    Tree
                        { values = Dict.empty
                        , children = Dict.singleton head (build tail)
                        }

        candidates : Dict ModuleName Candidates
        candidates =
            context.importListEnd
                |> Maybe.map
                    (\importListEnd ->
                        { moduleKey = moduleKey
                        , imports = context.imports
                        , importListEnd = importListEnd
                        , valueCandidates = context.valueCandidates
                        , callCandidates = context.callCandidates
                        }
                            |> Dict.singleton moduleName
                    )
                |> Maybe.withDefault Dict.empty
    in
    { genTree = build moduleName
    , candidates = candidates
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts l r =
    { genTree = mergeTree l.genTree r.genTree
    , candidates = Dict.union l.candidates r.candidates
    }


mergeTree : Tree -> Tree -> Tree
mergeTree (Tree l) (Tree r) =
    Tree
        { values = Dict.union l.values r.values
        , children =
            Dict.merge
                Dict.insert
                (\key lvalue rvalue -> Dict.insert key (mergeTree lvalue rvalue))
                Dict.insert
                l.children
                r.children
                Dict.empty
        }


finalEvaluation : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation context =
    context.candidates
        |> Dict.values
        |> List.concatMap (finalModuleEvaluation context.genTree)


finalModuleEvaluation : Tree -> Candidates -> List (Rule.Error { useErrorForModule : () })
finalModuleEvaluation genTree candidates =
    -- TODO: implement callCandidates
    let
        valueErrors : List (Rule.Error { useErrorForModule : () })
        valueErrors =
            candidates.valueCandidates
                |> List.map
                    (\candidate ->
                        case get genTree candidate.moduleName candidate.valueName of
                            Nothing ->
                                Rule.errorForModule candidates.moduleKey
                                    { message = "Use helpers for more readable and compact code"
                                    , details = [ "You can use local helpers to avoid directly needing Elm.value" ]
                                    }
                                    candidate.range

                            Just _ ->
                                let
                                    ( moduleName, importFixes ) =
                                        let
                                            rawModuleName : ModuleName
                                            rawModuleName =
                                                "Gen" :: candidate.moduleName
                                        in
                                        case Dict.get rawModuleName candidates.imports of
                                            Just (Just alias_) ->
                                                ( [ alias_ ], [] )

                                            Just Nothing ->
                                                ( rawModuleName, [] )

                                            Nothing ->
                                                ( rawModuleName
                                                , [ Fix.insertAt candidates.importListEnd
                                                        ("import " ++ String.join "." rawModuleName ++ "\n")
                                                  ]
                                                )

                                    fullName : String
                                    fullName =
                                        (moduleName ++ [ "values_", candidate.valueName ])
                                            |> String.join "."
                                in
                                Rule.errorForModuleWithFix candidates.moduleKey
                                    { message = "Use helpers for more readable and compact code"
                                    , details =
                                        [ "This usage of Elm.value can be replaced with " ++ fullName
                                        ]
                                    }
                                    candidate.range
                                    (Fix.replaceRangeBy candidate.range fullName :: importFixes)
                    )

        callErrors : List (Rule.Error { useErrorForModule : () })
        callErrors =
            candidates.callCandidates
                |> List.filterMap
                    (\candidate ->
                        case get genTree candidate.moduleName candidate.valueName of
                            Nothing ->
                                Rule.errorForModule candidates.moduleKey
                                    { message = "Use helpers for more readable and compact code"
                                    , details = [ "You can use local helpers to avoid directly needing Elm.apply" ]
                                    }
                                    (Range.combine [ candidate.applyRange, candidate.valueRange ])
                                    |> Just

                            Just arity ->
                                if List.length candidate.args /= arity then
                                    Nothing

                                else
                                    let
                                        moduleName : ModuleName
                                        moduleName =
                                            let
                                                rawModuleName : ModuleName
                                                rawModuleName =
                                                    "Gen" :: candidate.moduleName
                                            in
                                            case Dict.get rawModuleName candidates.imports of
                                                Just (Just alias_) ->
                                                    [ alias_ ]

                                                _ ->
                                                    rawModuleName

                                        fullName : String
                                        fullName =
                                            (moduleName ++ [ "call_", candidate.valueName ])
                                                |> String.join "."

                                        ( argsFixes, lastArgEnd ) =
                                            candidate.args
                                                |> List.foldl
                                                    (\argRange ( argsFixesAcc, argEndAcc ) ->
                                                        ( Fix.replaceRangeBy
                                                            { start = argEndAcc
                                                            , end = argRange.start
                                                            }
                                                            (if argEndAcc.row /= argRange.start.row then
                                                                "\n" ++ String.repeat argRange.start.column " " ++ "("

                                                             else
                                                                " ("
                                                            )
                                                            :: Fix.insertAt
                                                                argRange.end
                                                                ")"
                                                            :: argsFixesAcc
                                                        , argRange.end
                                                        )
                                                    )
                                                    ( [], candidate.listRange.start )

                                        combinedRange : Range
                                        combinedRange =
                                            Range.combine [ candidate.applyRange, candidate.valueRange ]
                                    in
                                    Rule.errorForModuleWithFix candidates.moduleKey
                                        { message = "Use helpers for more readable and compact code"
                                        , details =
                                            [ "This usage of Elm.apply can be replaced with " ++ fullName
                                            ]
                                        }
                                        combinedRange
                                        ([ Fix.replaceRangeBy combinedRange fullName
                                         , Fix.removeRange
                                            { start = lastArgEnd
                                            , end = candidate.listRange.end
                                            }
                                         ]
                                            ++ argsFixes
                                        )
                                        |> Just
                    )
    in
    -- Rule.errorForModule candidates.moduleKey
    --     { message = "Tree"
    --     , details = [ treeToString genTree ]
    --     }
    --     Range.empty
    --     ::
    valueErrors ++ callErrors



-- treeToString : Tree -> String
-- treeToString tree =
--     let
--         go : String -> Tree -> String
--         go indentation (Tree node) =
--             let
--                 valuesString : String
--                 valuesString =
--                     if Dict.isEmpty node.values then
--                         ""
--                     else
--                         indentation
--                             ++ " ["
--                             ++ (node.values
--                                     |> Dict.toList
--                                     |> List.filterMap
--                                         (\( name, arity ) ->
--                                             if String.endsWith "_" name then
--                                                 Nothing
--                                             else
--                                                 Just <| name ++ "/" ++ String.fromInt arity
--                                         )
--                                     |> String.join ", "
--                                )
--                             ++ "]"
--                 childrenStrings : List String
--                 childrenStrings =
--                     node.children
--                         |> Dict.toList
--                         |> List.filterMap
--                             (\( name, child ) ->
--                                 let
--                                     newIndent : String
--                                     newIndent =
--                                         if String.isEmpty indentation then
--                                             "- " ++ name
--                                         else
--                                             indentation ++ "." ++ name
--                                     childString : String
--                                     childString =
--                                         go newIndent child
--                                 in
--                                 if String.isEmpty childString then
--                                     Nothing
--                                 else
--                                     Just childString
--                             )
--             in
--             if String.isEmpty valuesString then
--                 String.join "\n" childrenStrings
--             else
--                 String.join "\n" <| valuesString :: childrenStrings
--     in
--     go "" tree


type alias ProjectContext =
    { genTree : Tree
    , candidates : Dict ModuleName Candidates
    }


type alias Candidates =
    { moduleKey : ModuleKey
    , imports : Dict ModuleName (Maybe String)
    , importListEnd : Location
    , valueCandidates : List ValueCandidate
    , callCandidates : List CallCandidate
    }


type alias ModuleContext =
    { isGen : Bool
    , values : Dict String Arity
    , imports : Dict ModuleName (Maybe String)
    , importListEnd : Maybe Location
    , valueCandidates : List ValueCandidate
    , callCandidates : List CallCandidate
    }


{-| Candidate for being replaced by `Gen.Foo.values_.name`
-}
type alias ValueCandidate =
    { moduleName : ModuleName
    , valueName : String
    , range : Range
    }


{-| Candidate for being replaced by `Gen.Foo.call_.name`
-}
type alias CallCandidate =
    { moduleName : ModuleName
    , valueName : String
    , applyRange : Range
    , valueRange : Range
    , listRange : Range
    , args : List Range
    }


type Tree
    = Tree
        { values : Dict String Arity
        , children : Dict String Tree
        }


leaf : Dict String Arity -> Tree
leaf values =
    Tree
        { values = values
        , children = Dict.empty
        }


type alias Arity =
    Int


initialContext : ProjectContext
initialContext =
    { genTree = leaf Dict.empty
    , candidates = Dict.empty
    }


get : Tree -> ModuleName -> String -> Maybe Int
get genTree moduleName valueName =
    let
        go : Tree -> List String -> Maybe Arity
        go (Tree node) queue =
            case queue of
                [] ->
                    Dict.get valueName node.values

                head :: tail ->
                    case Dict.get head node.children of
                        Just child ->
                            go child tail

                        Nothing ->
                            Nothing
    in
    go genTree ("Gen" :: moduleName)


importVisitor : Node Import -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
importVisitor (Node range import_) context =
    ( []
    , { context
        | imports =
            context.imports
                |> Dict.insert
                    (Node.value import_.moduleName)
                    (Maybe.andThen (List.head << Node.value) import_.moduleAlias)
        , importListEnd =
            { row = range.end.row + 1
            , column = 0
            }
                |> Just
      }
    )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    if context.isGen then
        ( []
        , { context
            | values =
                declarations
                    |> List.filterMap
                        (\(Node _ declaration) ->
                            case declaration of
                                Declaration.FunctionDeclaration function ->
                                    let
                                        implementation : FunctionImplementation
                                        implementation =
                                            Node.value function.declaration
                                    in
                                    ( Node.value implementation.name, List.length implementation.arguments )
                                        |> Just

                                _ ->
                                    Nothing
                        )
                    |> Dict.fromList
          }
        )

    else
        ( [], context )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor (Node range node) context =
    if context.isGen then
        ( [], context )

    else
        case node of
            Expression.Application [ Node _ (Expression.FunctionOrValue [ "Elm" ] "value"), Node _ (Expression.RecordExpr fields) ] ->
                let
                    getField : String -> Maybe (Node Expression)
                    getField key =
                        List.Extra.findMap
                            (\(Node _ ( Node _ fieldName, fieldValue )) ->
                                if fieldName == key then
                                    Just fieldValue

                                else
                                    Nothing
                            )
                            fields

                    importFrom : Maybe (List String)
                    importFrom =
                        getField "importFrom"
                            |> Maybe.andThen asList
                            |> Maybe.andThen (Maybe.Extra.combineMap asString)

                    name : Maybe String
                    name =
                        getField "name"
                            |> Maybe.andThen asString
                in
                case Maybe.map2 Tuple.pair importFrom name of
                    Just ( moduleName, valueName ) ->
                        if String.startsWith "w3_" valueName then
                            -- Lamdera-generated function, no binding will be available
                            ( [], context )

                        else
                            let
                                candidate : ValueCandidate
                                candidate =
                                    { moduleName = moduleName
                                    , valueName = valueName
                                    , range = range
                                    }
                            in
                            ( []
                            , { context | valueCandidates = candidate :: context.valueCandidates }
                            )

                    _ ->
                        -- If moduleName and valueName are not constants it actually makes sense to use Elm.value
                        ( [], context )

            Expression.Application [ Node applyRange (Expression.FunctionOrValue [ "Elm" ] "apply"), Node valueRange (Expression.RecordAccess (Node _ (Expression.FunctionOrValue ("Gen" :: moduleName) "values_")) (Node _ valueName)), Node listRange (Expression.ListExpr args) ] ->
                let
                    candidate : CallCandidate
                    candidate =
                        { moduleName = moduleName
                        , valueName = valueName
                        , applyRange = applyRange
                        , valueRange = valueRange
                        , listRange = listRange
                        , args = List.map Node.range args
                        }
                in
                ( []
                , { context | callCandidates = candidate :: context.callCandidates }
                )

            _ ->
                ( [], context )


asString : Node Expression -> Maybe String
asString (Node _ expr) =
    case expr of
        Expression.Literal child ->
            Just child

        _ ->
            Nothing


asList : Node Expression -> Maybe (List (Node Expression))
asList (Node _ expr) =
    case expr of
        Expression.ListExpr children ->
            Just children

        _ ->
            Nothing
