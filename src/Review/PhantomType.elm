module Review.PhantomType exposing (forbid)

{-| Reports choice `type` parameters that aren't used in the definition (often called "phantom types").

    import NoUnused.CustomTypeConstructorArgs
    import NoUnused.CustomTypeConstructors
    import Review.PhantomType

    config =
        [ Review.PhantomType.forbid

        -- to catch variables in unused parts of the type
        , NoUnused.CustomTypeConstructors.rule []
        , NoUnused.CustomTypeConstructorArgs.rule
        ]

  - 🧩 [`NoUnused.CustomTypeConstructorArgs`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructorArgs)
  - 🧩 [`NoUnused.CustomTypeConstructors`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructors)

@docs forbid

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation
import FastDict exposing (Dict)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule exposing (Rule)
import Set exposing (Set)


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : Elm.Syntax.ModuleName.ModuleName
    , moduleTypeAliases : Dict String TypeAliasContext
    , moduleChoiceTypes : Dict String ChoiceTypeContext
    , imported : ProjectContext
    }


type alias ProjectContext =
    { expansions : Expansions }


{-| Named types that itself can be expanded to more types
-}
type alias Expansions =
    Dict
        ( Elm.Syntax.ModuleName.ModuleName, String )
        { parameters : List String
        , types : List Elm.Syntax.TypeAnnotation.TypeAnnotation
        }


type alias TypeAliasContext =
    { parameters : List String
    , type_ : Elm.Syntax.TypeAnnotation.TypeAnnotation
    }


type alias ChoiceTypeContext =
    { parameters : List (Node String)
    , variants :
        Dict String (List Elm.Syntax.TypeAnnotation.TypeAnnotation)
    }


{-| [`Rule`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#Rule) to report phantom types.

More on the why and the alternatives in the [readme](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-phantom-type/latest/)

-}
forbid : Rule
forbid =
    Review.Rule.newProjectRuleSchema "Review.PhantomType.forbid" initialProjectContext
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
            , foldProjectContexts = projectContextsMerge
            }
        |> Review.Rule.fromProjectRuleSchema


initialProjectContext : ProjectContext
initialProjectContext =
    { expansions = FastDict.empty }


projectContextsMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextsMerge =
    \a b ->
        { expansions = FastDict.union a.expansions b.expansions }


choiceTypesFromModuleToExpansion :
    Elm.Syntax.ModuleName.ModuleName
    -> (Dict String ChoiceTypeContext -> Expansions)
choiceTypesFromModuleToExpansion moduleName =
    \moduleChoiceTypes ->
        moduleChoiceTypes
            |> FastDict.toList
            |> List.map
                (\( unqualifiedName, choiceType ) ->
                    ( ( moduleName, unqualifiedName )
                    , { parameters = choiceType.parameters |> List.map Elm.Syntax.Node.value
                      , types = choiceType.variants |> FastDict.values |> List.concat
                      }
                    )
                )
            |> FastDict.fromList


typeAliasesFromModuleToExpansion :
    Elm.Syntax.ModuleName.ModuleName
    -> (Dict String TypeAliasContext -> Expansions)
typeAliasesFromModuleToExpansion moduleName =
    \moduleTypeAliases ->
        moduleTypeAliases
            |> FastDict.toList
            |> List.map
                (\( unqualifiedName, typeAlias ) ->
                    ( ( moduleName, unqualifiedName )
                    , { parameters = typeAlias.parameters
                      , types = [ typeAlias.type_ ]
                      }
                    )
                )
            |> FastDict.fromList


moduleToProjectContextCreator : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContextCreator =
    Review.Rule.initContextCreator
        (\moduleName moduleContext ->
            { expansions =
                FastDict.union
                    (moduleContext.moduleTypeAliases |> typeAliasesFromModuleToExpansion moduleName)
                    (moduleContext.moduleChoiceTypes |> choiceTypesFromModuleToExpansion moduleName)
            }
        )
        |> Review.Rule.withModuleName


projectToModuleContextCreator : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContextCreator =
    Review.Rule.initContextCreator
        (\lookupTable moduleName projectContext ->
            { lookupTable = lookupTable
            , moduleName = moduleName
            , moduleTypeAliases = FastDict.empty
            , moduleChoiceTypes = FastDict.empty
            , imported = projectContext
            }
        )
        |> Review.Rule.withModuleNameLookupTable
        |> Review.Rule.withModuleName


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Review.Rule.Error {}), ModuleContext )
declarationListVisitor declarationList context =
    let
        typeAliases : Dict String TypeAliasContext
        typeAliases =
            declarationList
                |> List.filterMap (\(Node _ d) -> d |> declarationToTypeAliasContext)
                |> FastDict.fromList

        choiceTypes : Dict String ChoiceTypeContext
        choiceTypes =
            choiceTypesList |> FastDict.fromList

        choiceTypesList : List ( String, ChoiceTypeContext )
        choiceTypesList =
            declarationList
                |> List.filterMap (\(Node _ d) -> d |> declarationToChoiceTypeContext)

        expansions : Expansions
        expansions =
            FastDict.union context.imported.expansions
                (FastDict.union
                    (typeAliases |> typeAliasesFromModuleToExpansion context.moduleName)
                    (choiceTypes |> choiceTypesFromModuleToExpansion context.moduleName)
                )
    in
    ( choiceTypesList
        |> List.concatMap
            (\( unqualifiedName, choiceType ) ->
                let
                    usedVariables : Set String
                    usedVariables =
                        Elm.Syntax.TypeAnnotation.Typed
                            (Elm.Syntax.Node.empty ( [], unqualifiedName ))
                            (choiceType.parameters
                                |> List.map (Elm.Syntax.Node.map Elm.Syntax.TypeAnnotation.GenericType)
                            )
                            |> typeUsedVariables
                                { moduleNameLookup = context.lookupTable
                                , useModuleName = context.moduleName
                                , referredFrom = Set.empty
                                , expansions = expansions
                                }
                            |> .knownToBeUsed
                in
                choiceType.parameters
                    |> List.filterMap
                        (\(Node parameterRange parameter) ->
                            if usedVariables |> Set.member parameter then
                                Nothing

                            else
                                Review.Rule.error errorInfo parameterRange |> Just
                        )
            )
    , { context
        | moduleTypeAliases = typeAliases
        , moduleChoiceTypes = choiceTypes
      }
    )


errorInfo : { message : String, details : List String }
errorInfo =
    { message = "variable not used in the definition"
    , details =
        [ """This is often called "phantom type" because the type argument has no effect on values that can be constructed using its variants.
These types are usually used to provide more information on specific constructed values in order to restrict their use."""
        , """These types can be somewhat complex and tricky to use well and they don't give you the usual rewards of type-safety either."""
        , """If you were not aware of this feature when writing this type, it's safe to remove the parameter or use it somewhere.
If you intentionally used this phantom type, I suggest looking at these alternatives: https://dark.elm.dmy.fr/packages/lue-bird/elm-review-phantom-type/latest#but-what-are-the-alternatives"""
        ]
    }


typeUsedVariables :
    { moduleNameLookup : ModuleNameLookupTable
    , useModuleName : Elm.Syntax.ModuleName.ModuleName
    , referredFrom : Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , expansions : Expansions
    }
    ->
        (Elm.Syntax.TypeAnnotation.TypeAnnotation
         ->
            { knownToBeUsed : Set String
            , referredBackTo :
                Dict
                    ( Elm.Syntax.ModuleName.ModuleName, String )
                    (List Elm.Syntax.TypeAnnotation.TypeAnnotation)
            }
        )
typeUsedVariables context =
    -- IGNORE TCO
    let
        typeNodesUsedVariableList :
            Set ( Elm.Syntax.ModuleName.ModuleName, String )
            ->
                (List (Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
                 ->
                    { knownToBeUsed : Set String
                    , referredBackTo :
                        Dict
                            ( Elm.Syntax.ModuleName.ModuleName, String )
                            (List Elm.Syntax.TypeAnnotation.TypeAnnotation)
                    }
                )
        typeNodesUsedVariableList newRefersFrom =
            let
                newContext :
                    { moduleNameLookup : ModuleNameLookupTable
                    , useModuleName : Elm.Syntax.ModuleName.ModuleName
                    , referredFrom : Set ( Elm.Syntax.ModuleName.ModuleName, String )
                    , expansions : Expansions
                    }
                newContext =
                    { context | referredFrom = Set.union context.referredFrom newRefersFrom }
            in
            \typeNodes ->
                typeNodes
                    |> List.foldl
                        (\(Node _ type_) soFar ->
                            let
                                variables :
                                    { knownToBeUsed : Set String
                                    , referredBackTo :
                                        Dict
                                            ( Elm.Syntax.ModuleName.ModuleName, String )
                                            (List Elm.Syntax.TypeAnnotation.TypeAnnotation)
                                    }
                                variables =
                                    type_ |> typeUsedVariables newContext
                            in
                            { knownToBeUsed = Set.union soFar.knownToBeUsed variables.knownToBeUsed
                            , referredBackTo = FastDict.union soFar.referredBackTo variables.referredBackTo
                            }
                        )
                        { knownToBeUsed = Set.empty, referredBackTo = FastDict.empty }
    in
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.Unit ->
                { knownToBeUsed = Set.empty, referredBackTo = FastDict.empty }

            Elm.Syntax.TypeAnnotation.GenericType variable ->
                { knownToBeUsed = variable |> Set.singleton, referredBackTo = FastDict.empty }

            Elm.Syntax.TypeAnnotation.Typed (Node nameRange ( _, unqualifiedName )) arguments ->
                let
                    moduleName : Elm.Syntax.ModuleName.ModuleName
                    moduleName =
                        case Review.ModuleNameLookupTable.fullModuleNameAt context.moduleNameLookup nameRange of
                            Nothing ->
                                context.useModuleName

                            Just moduleNameFound ->
                                moduleNameFound
                in
                if context.referredFrom |> Set.member ( moduleName, unqualifiedName ) then
                    { knownToBeUsed = Set.empty
                    , referredBackTo =
                        FastDict.singleton ( moduleName, unqualifiedName )
                            (arguments |> List.map Elm.Syntax.Node.value)
                    }

                else
                    case context.expansions |> FastDict.get ( moduleName, unqualifiedName ) of
                        -- type is imported from dependencies
                        Nothing ->
                            arguments |> typeNodesUsedVariableList Set.empty

                        Just expansion ->
                            let
                                untilBackRefers :
                                    { knownToBeUsed : Set String
                                    , referredBackTo :
                                        Dict
                                            ( Elm.Syntax.ModuleName.ModuleName, String )
                                            (List Elm.Syntax.TypeAnnotation.TypeAnnotation)
                                    }
                                untilBackRefers =
                                    expansion.types
                                        |> List.map
                                            (\expansionType ->
                                                expansionType
                                                    |> typeFillInVariables
                                                        (List.map2 (\parameter (Node _ fill) -> ( parameter, fill ))
                                                            expansion.parameters
                                                            arguments
                                                            |> FastDict.fromList
                                                        )
                                                    |> Elm.Syntax.Node.empty
                                            )
                                        |> typeNodesUsedVariableList (( moduleName, unqualifiedName ) |> Set.singleton)
                            in
                            case untilBackRefers.referredBackTo |> FastDict.get ( moduleName, unqualifiedName ) of
                                Nothing ->
                                    { knownToBeUsed = untilBackRefers.knownToBeUsed
                                    , referredBackTo = untilBackRefers.referredBackTo
                                    }

                                Just referBack ->
                                    let
                                        unknownToBeUsed : List (Maybe String)
                                        unknownToBeUsed =
                                            arguments
                                                |> List.map
                                                    (\(Node _ arg) ->
                                                        case arg |> typeToVariable of
                                                            Nothing ->
                                                                Nothing

                                                            Just variable ->
                                                                if untilBackRefers.knownToBeUsed |> Set.member variable then
                                                                    Nothing

                                                                else
                                                                    variable |> Just
                                                    )

                                        knownThanksToReferBack : Set String
                                        knownThanksToReferBack =
                                            -- check if previously unknown variables are used in arguments that are known
                                            List.map2
                                                (\argsBackReference unknownToBeUsedVariable ->
                                                    case unknownToBeUsedVariable of
                                                        Just _ ->
                                                            Set.empty

                                                        Nothing ->
                                                            argsBackReference |> typeUsedVariables context |> .knownToBeUsed
                                                )
                                                referBack
                                                unknownToBeUsed
                                                |> listSetUnion
                                    in
                                    { knownToBeUsed = Set.union untilBackRefers.knownToBeUsed knownThanksToReferBack
                                    , referredBackTo = untilBackRefers.referredBackTo |> FastDict.remove ( moduleName, unqualifiedName )
                                    }

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> typeNodesUsedVariableList Set.empty

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields
                    |> List.map (\(Node _ ( _, valueTypeNode )) -> valueTypeNode)
                    |> typeNodesUsedVariableList Set.empty

            Elm.Syntax.TypeAnnotation.GenericRecord extendedVariableNode (Node _ fields) ->
                (extendedVariableNode |> Elm.Syntax.Node.map Elm.Syntax.TypeAnnotation.GenericType)
                    :: (fields |> List.map (\(Node _ ( _, valueTypeNode )) -> valueTypeNode))
                    |> typeNodesUsedVariableList Set.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation fromNode toNode ->
                [ fromNode, toNode ] |> typeNodesUsedVariableList Set.empty


typeToVariable :
    Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Maybe String
typeToVariable =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType variable ->
                variable |> Just

            _ ->
                Nothing


typeFillInVariables :
    Dict String Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        (Elm.Syntax.TypeAnnotation.TypeAnnotation
         -> Elm.Syntax.TypeAnnotation.TypeAnnotation
        )
typeFillInVariables variablesToFillIn =
    typeMap
        (\type_ ->
            case type_ of
                Elm.Syntax.TypeAnnotation.GenericType variable ->
                    case variablesToFillIn |> FastDict.get variable of
                        Nothing ->
                            Elm.Syntax.TypeAnnotation.GenericType variable

                        Just fill ->
                            fill

                nonVariableType ->
                    nonVariableType
        )


{-| Map it, then all its sub-types, all the way down
-}
typeMap :
    (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeMap typeChange =
    let
        step : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        step =
            Elm.Syntax.Node.map (\stepType -> stepType |> typeMap typeChange)
    in
    -- IGNORE TCO
    \type_ ->
        case type_ |> typeChange of
            Elm.Syntax.TypeAnnotation.Unit ->
                Elm.Syntax.TypeAnnotation.Unit

            Elm.Syntax.TypeAnnotation.GenericType name ->
                Elm.Syntax.TypeAnnotation.GenericType name

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (input |> step) (output |> step)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                Elm.Syntax.TypeAnnotation.Tupled (parts |> List.map step)

            Elm.Syntax.TypeAnnotation.Record fields ->
                Elm.Syntax.TypeAnnotation.Record
                    (fields |> List.map (Elm.Syntax.Node.map (\( name, value ) -> ( name, value |> step ))))

            Elm.Syntax.TypeAnnotation.GenericRecord extended fields ->
                Elm.Syntax.TypeAnnotation.GenericRecord extended
                    (fields
                        |> Elm.Syntax.Node.map
                            (List.map (Elm.Syntax.Node.map (\( name, value ) -> ( name, value |> step ))))
                    )

            Elm.Syntax.TypeAnnotation.Typed nameNode arguments ->
                Elm.Syntax.TypeAnnotation.Typed nameNode (arguments |> List.map step)


listSetUnion : List (Set comparable) -> Set comparable
listSetUnion =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    Set.union soFar element
                )
                Set.empty


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
                , { parameters = syntaxChoiceType.generics
                  , variants =
                        syntaxChoiceType.constructors
                            |> List.map
                                (\(Node _ syntaxVariant) ->
                                    ( syntaxVariant.name |> Elm.Syntax.Node.value
                                    , syntaxVariant.arguments |> List.map Elm.Syntax.Node.value
                                    )
                                )
                            |> FastDict.fromList
                  }
                )
                    |> Just

            _ ->
                Nothing
