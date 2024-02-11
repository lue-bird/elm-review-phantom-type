module Type.LocalExtra exposing (fillInVariables, toVariable)

import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.TypeAnnotation
import FastDict exposing (Dict)


toVariable :
    Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Maybe String
toVariable =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType variable ->
                variable |> Just

            _ ->
                Nothing


fillInVariables :
    Dict String Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        (Elm.Syntax.TypeAnnotation.TypeAnnotation
         -> Elm.Syntax.TypeAnnotation.TypeAnnotation
        )
fillInVariables variablesToFillIn =
    map
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
map :
    (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
map typeChange =
    let
        step : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        step =
            Elm.Syntax.Node.map (\stepType -> stepType |> map typeChange)
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
