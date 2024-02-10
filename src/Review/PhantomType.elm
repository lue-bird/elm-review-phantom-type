module Review.PhantomType exposing (forbid)

{-| Reports choice `type` parameters that aren't used in the definition (often called "phantom types").

    config =
        [ Review.PhantomType.forbid
        ]

@docs forbid

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation
import Review.Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Error, Rule)
import Set exposing (Set)


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleTypeAliases : Dict String TypeAliasContext
    , moduleChoiceTypes : Dict String ChoiceTypeContext
    , imported : ProjectContext
    }


type alias ProjectContext =
    Dict
        Elm.Syntax.ModuleName.ModuleName
        { moduleTypeAliases : Dict String TypeAliasContext
        , moduleChoiceTypes : Dict String ChoiceTypeContext
        }


type alias TypeAliasContext =
    { parameters : List String
    , type_ : Elm.Syntax.TypeAnnotation.TypeAnnotation
    }


type alias ChoiceTypeContext =
    { parameters : List String
    , variants :
        Dict String (List Elm.Syntax.TypeAnnotation.TypeAnnotation)
    }


{-| The rule performing the given [`Upgrade`](#Upgrade)
-}
forbid : Rule
forbid =
    Review.Rule.newProjectRuleSchema "Review.PhantomType.forbid" Dict.empty
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleVisitor
            (\moduleRuleSchema ->
                moduleRuleSchema
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarationList context -> declarationListVisitor declarationList context)
            )
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectToModuleContextCreator
            , fromModuleToProject = moduleToProjectContextCreator
            , foldProjectContexts = Dict.union
            }
        |> Review.Rule.fromProjectRuleSchema


moduleToProjectContextCreator : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContextCreator =
    Review.Rule.initContextCreator
        (\moduleName moduleContext ->
            Dict.singleton moduleName
                { moduleTypeAliases = moduleContext.moduleTypeAliases
                , moduleChoiceTypes = moduleContext.moduleChoiceTypes
                }
        )
        |> Review.Rule.withModuleName


projectToModuleContextCreator : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContextCreator =
    Review.Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , moduleTypeAliases = Dict.empty
            , moduleChoiceTypes = Dict.empty
            , imported = projectContext
            }
        )
        |> Review.Rule.withModuleNameLookupTable


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Review.Rule.Error {}), ModuleContext )
declarationListVisitor declarationList context =
    let
        typeAliases : Dict String TypeAliasContext
        typeAliases =
            declarationList
                |> List.filterMap (\(Node _ d) -> d |> declarationToTypeAliasContext)
                |> Dict.fromList

        choiceTypes : Dict String ChoiceTypeContext
        choiceTypes =
            declarationList
                |> List.filterMap (\(Node _ d) -> d |> declarationToChoiceTypeContext)
                |> Dict.fromList
    in
    ( Debug.todo ""
    , { context
        | moduleTypeAliases = typeAliases
        , moduleChoiceTypes = choiceTypes
      }
    )


declarationToTypeAliasContext : Elm.Syntax.Declaration.Declaration -> Maybe ( String, TypeAliasContext )
declarationToTypeAliasContext =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAlias ->
                ( syntaxTypeAlias.name |> Elm.Syntax.Node.value
                , { parameters = syntaxTypeAlias.generics |> List.map Elm.Syntax.Node.value
                  , type_ = syntaxTypeAlias.typeAnnotation |> Elm.Syntax.Node.value
                  }
                )
                    |> Just

            _ ->
                Nothing


declarationToChoiceTypeContext : Elm.Syntax.Declaration.Declaration -> Maybe ( String, ChoiceTypeContext )
declarationToChoiceTypeContext =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceType ->
                ( syntaxChoiceType.name |> Elm.Syntax.Node.value
                , { parameters = syntaxChoiceType.generics |> List.map Elm.Syntax.Node.value
                  , variants =
                        syntaxChoiceType.constructors
                            |> List.map
                                (\(Node _ syntaxVariant) ->
                                    ( syntaxVariant.name |> Elm.Syntax.Node.value
                                    , syntaxVariant.arguments |> List.map Elm.Syntax.Node.value
                                    )
                                )
                            |> Dict.fromList
                  }
                )
                    |> Just

            _ ->
                Nothing
