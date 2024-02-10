# elm-review-phantom-type

[`Review.PhantomType.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-phantom-type/1.0.0/Review-PhantomType#forbid)
reports choice `type` parameters that aren't used in the definition – often called "phantom types".

If you want to learn more about phantom types first, some recommends:
  - [podcast episode "Phantom Builder Pattern" by elm radio](https://elm-radio.com/episode/phantom-builder/)
  - [talk "The phantom builder pattern" by Jeroen Engels](https://www.youtube.com/watch?v=Trp3tmpMb-o)

```elm
import Review.Rule
import Review.PhantomType
import NoUnused.CustomTypeConstructors
import NoUnused.CustomTypeConstructorArgs

config : List Review.Rule.Rule
config =
    [ Review.PhantomType.forbid

    -- to catch unused type variables
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    ]
```
  - 🧩 [`NoUnused.CustomTypeConstructorArgs`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructorArgs)
  - 🧩 [`NoUnused.CustomTypeConstructors`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructors)

## why?

Claim: "phantom types are hopefully safe, but without the rewards" – worse than even opaque types.

  - phantom types are not simple.
    How much time would it take you to teach someone extensible phantom record type builders
    in a way that they could write an API without ways to bypass the types?
    It certainly risks increasing the burden of entry for users of your API.

  - phantom types are tricky to get right – not great for a _type_ which is supposed to be clear and provide safety.
    To the untrained eye, they can seem somewhat magical, even.
    Like relying on overriding field value types in an extended record phantom argument and so on.
    Worst of all, if you e.g. accidentally provide the wrong phantom type argument (like by misspelling an existing variable),
    there will be no friendly compiler that has your back.
    
  - the value does not know as much as your type.
    If you know opaque types, you know this problem.
    ```elm
    type Button constraints
        = Button { ..., label : Maybe Label }
    
    type Label
        = Text String
        | Icon Icon
    
    type LabelMissing
        = LabelMissing Never
    
    type LabelPresent
        = LabelPresent Never

    create : Button LabelMissing
    withText : String -> (Button LabelMissing -> Button LabelPresent)
    withIcon : Icon -> (Button LabelMissing -> Button LabelPresent)
    
    toHtml : Button LabelPresent -> Html msg
    toHtml = \button ->
        ...
        case button.label of
            Just (Text text) -> text |> Html.text
            Just (Icon icon) -> icon |> Icon.toHtml
            Nothing ->
                ??
                -- welp, this should never happen
                -- so why do I need to handle this?
    ```
    compare with e.g.
    ```elm
    type Button
        = Button { ..., label : Label }
    
    type Label
        = Text String
        | Icon Icon
    
    labelled : Label -> Button
    
    toHtml : Button -> Html msg
    toHtml = \button ->
        ...
        case button.label of
            Text text -> text |> Html.text
            Icon icon -> icon |> Icon.toHtml
    ```
    It's not about the specific API here, it's about the fact that you can safely read the `label`.

## but what are the alternatives?

from stupidly obvious to powerful

  - is limiting the choice worth it here? Like, what's the harm in allowing e.g.
      - your builder to set a background color after you've already done so
      - rules that have no visitors
    
    Maybe you'll also find cases where e.g. having no visitors is useful like with insight rules that only use info from the initial context creator.
    In any case, just because you can't find a use-case for a value that isn't harmful, why all the complexity to ban it?

  - if you already have a clear idea for the shape of an API and it seems impossible to actualize without phantom types,
    try asking yourself which parts of the API design are functional and which parts are the "how it looks".
    You _could_ likely even emulate your idea without phantom types but maybe...

  - can you model the same by adding more choice `type`s?
    An example based on [`WebGL.Texture.Resize`](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL-Texture#Resize)
    ```elm
    -- module WebGL.Texture exposing (Options, Resize, Smaller, Bigger, linear, nearest, nearestMipmapLinear, ...)
    type alias Options =
        { ...
        , magnify : Resize Bigger
        , minify : Resize Smaller
        }

    type Resize scaling = ...
    
    type Smaller = Smaller
    type Bigger = Bigger
    
    linear : Resize scaling
    nearest : Resize scaling
    nearestMipmapLinear : Resize Smaller
    ```
    instead, try for example
    ```elm
    -- module WebGL.Texture exposing (Options, Magnify(..), Minify(..), ...)
    type alias Options =
        { ...
        , magnify : Magnify
        , minify : Minify
        }

    type Magnify
        = MagnifyLinear
        | MagnifyNearest
    
    type Minify
        = MinifyLinear
        | MinifyNearest
        | MinifyMipmapLinear
    ```
    A really good example on how to do this well can be seen in [`elm-community/typed-svg`](https://dark.elm.dmy.fr/packages/elm-community/typed-svg/latest/TypedSvg-Types)
    where many types may share some variants like "inherit", "none" and "auto" but in reality, there isn't really one bigger connection uniting all these types.

    It _can_ make sense to make a type from shared variants in certain contexts.
    If you can find a name for it, that's a good indicator.
    ```elm
    -- module WebGL.Texture exposing (Resize, SimpleResize(..), Minify(..), ...)
    type alias Options =
        { ...
        , magnify : SimpleResize
        , minify : Minify
        }
    
    type SimpleResize
        = Linear
        | Nearest
    
    type Minify
        = MinifySimple SimpleResize
        | MinifyMipmapLinear
    ```
    usually though, this is just brain-brain trying to be too clever.

  - can you model the builder differently?
    Based on [the button example from the talk "The phantom builder pattern" by Jeroen Engels](https://www.youtube.com/watch?v=Trp3tmpMb-o&t=365s)
    ```elm
    -- module Button exposing (Button, Behaviour, BehaviourMissing, BehaviourPresent, new, withOnClick, withDisabled)
    type Button constraints msg
        = Button
            { ...
            , behaviour : Behaviour msg
            }
    
    type Behaviour msg
        = Disabled
        | OnClick msg
    
    type BehaviourMissing
        = BehaviourMissing Never
    
    type BehaviourPresent
        = BehaviourPresent Never
    
    create : Button OnClickOrDisabledMissing msg
    withDisabled :
        Button BehaviourMissing msg
        -> Button BehaviourPresent msg
    withOnClick :
        msg
        -> (Button BehaviourMissing msg
            -> Button BehaviourPresent msg
           )
    ```
    instead, try for example unifying builder helpers that are expected to be called in order
    ```elm
    -- module Button exposing (Button, Behaviour(..), create)
    type Button constraints msg
        = Button
            { ...
            , behaviour : Behaviour msg
            }
    
    type Behaviour msg
        = Disabled
        | OnClick msg
    
    create : { behaviour : Behaviour msg } -> Button msg
    ```
  
  - model each builder "state" as a separate type.
    Here's an example slightly similar to [`Review.Rule.withModuleVisitor`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#withModuleVisitor)
    into [`Review.Rule.withModuleContext`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#withModuleContext)
    ```elm
    type ReviewRuleSchema constraints = ...

    type ConversionsAndFoldMissing
        = ConversionsAndFoldMissing Never

    type ConversionsAndFoldNotMissing
        = ConversionsAndFoldNotMissing Never
    
    withModuleVisitor :
        ...
        -> (ReviewRuleSchema ConversionsAndFoldNotMissing
            -> ReviewRuleSchema ConversionsAndFoldMissing
           )
    
    withConversionsAndFold :
        ...
        -> (ReviewRuleSchema ConversionsAndFoldMissing
            -> ReviewRuleSchema ConversionsAndFoldNotMissing
           )
    ```
    instead, try
    ```elm
    type ReviewRuleSchema = ...
    type ReviewRuleSchemaWithConversionsAndFoldMissing = ...
    
    withModuleVisitor :
        ...
        -> (ReviewRuleSchema
            -> ReviewRuleSchemaWithConversionsAndFoldMissing
           )
    
    withConversionsAndFold :
        ...
        -> (ReviewRuleSchemaWithConversionsAndFoldMissing
            -> ReviewRuleSchema
           )
    ```
    obviously this has it's limits and is mostly useful if you explicitly need a specific kind of call next.
    So if you want to use a specific call for different states, you'll need another method.

  - use `Never` to mark certain states as forbidden. Pretty underrated IMO.
    An example similar to [`Json.Decode.Attempt`](https://dark.elm.dmy.fr/packages/MackeyRMS/json-decode-attempt/latest/Json-Decode-Attempt)
    ```elm
    type JsonDecoder parsed recoverable
        = JsonDecoder (Json.Decode.Value -> Result Error parsed)
    
    type Recoverable = Recoverable
    type Fallible = Fallible

    decode : JsonDecoder Recoverable parsed -> (Json.Decode.Value -> parsed)
    decode (JsonDecoder jsonDecode) = \jsonValue ->
        case jsonValue |> jsonDecode of
            Ok parsed ->
                parsed
            
            Err _ ->
                ??? just throw a runtime error I guess
                jsonValue |> decode (JsonDecoder jsonDecode)
    ```
    instead, try
    ```elm
    type JsonDecoder parsed error
        = JsonDecoder (Json.Decode.Value -> Result error parsed)
    
    decode : JsonDecoder parsed Never -> (Json.Decode.Value -> parsed)
    decode (JsonDecoder jsonDecode) = \jsonValue ->
        case jsonValue |> jsonDecode of
            Ok parsed ->    
                parsed
            
            Err ever ->
                never ever
    ```
    see [`Basics.never`](https://dark.elm.dmy.fr/packages/elm/core/latest/Basics#never)
    on how this is different from before: `Never` is impossible to construct, even internally.

    It's common that builders require _at least 1_ call to some helper. Something like
    ```elm
    create ...
        |> and A ...
        |> and B ...
        |> and C ...
    ```
    obviously, you can and should at least consider doing something like
    ```elm
    create ...
        |> andStartWith A ...
        |> and B ...
        |> and C ...
    -- or
    createAndStartWith ...
        A ...
        |> and B ...
        |> and C ...
    ```
    but admittedly this can _look_ ugly.

    To be able to re-use `and` from both states, we can add a type variable that determines what we know about the builder being "empty": either `Never` or 🧩 [`Possibly`](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/Possibly)
    ```elm
    -- module Enum exposing (Enum, EnumBuilder, ...)
    type alias Enum value =
        EnumEmptiable HasMembers value (value -> { name : String, index : Int })
    
    type EnumBuilder constraints value toInfo =
        EnumEmptiable
            { toInfo : toInfo
            , list : List value
            }
    
    type HasNoMembers
        = HasNoMembers Never
    
    type HasMembers
        = HasMembers Never
    
    create : toInfo -> EnumBuilder HasNoMembers value_ toInfo
    create toInfo =
        { toInfo = toInfo, list = Emptiable.empty }
    
    and :
        value
        -> String
        -> (EnumBuilder constraints_ value ({ name : String, index : Int } -> toInfo)
            -> EnumBuilder HasMembers value toInfo
           )
    and value name = \enumSoFar ->
        { toInfo =
            enumSoFar.toInfo { name = name, index = enumSoFar.list |> Stack.length }
        , list = enumSoFar.list |> Stack.onTopLay value
        }
    
    randomlyChooseOne : Enum value -> Random.Generator value
    randomlyChooseOne enum =
        case enum.list of
            head :: tail ->
                Random.uniform head tail
            
            [] ->
                ??
    
    example : Enum Order
    example =
        create
            (\lt eq gt order ->
                case order of
                    LT -> lt
                    EQ -> eq
                    GT -> gt
            )
            |> and LT "LT"
            |> and EQ "EQ"
            |> and GT "GT"
    ```
      - 🧩 [`Random`](https://dark.elm.dmy.fr/packages/elm/random/latest/Random)
    
    We know the `list` will never be empty but the compiler doesn't. Instead, try
    ```elm
    -- module Enum exposing (Enum, EnumBuilder(..), ...)
    type alias Enum value =
        EnumEmptiable Never value (value -> { name : String, index : Int })
    
    type alias EnumEmptiable emptyPossiblyOrNever value toInfo =
        { toInfo : toInfo
        , list : Emptiable (Stacked value) emptyPossiblyOrNever
        }
    
    create : toInfo -> EnumEmptiable Possibly value_ toInfo
    create toInfo =
        { toInfo = toInfo, list = Emptiable.empty }
    
    and :
        value
        -> String
        -> (EnumEmptiable emptyPossiblyOrNever_ value ({ name : String, index : Int } -> toInfo)
            -> EnumEmptiable never_ value toInfo
           )
    and value name = \enumSoFar ->
        { toInfo =
            enumSoFar.toInfo { name = name, index = enumSoFar.list |> Stack.length }
        , list = enumSoFar.list |> Stack.onTopLay value
        }
    
    randomlyChooseOne : Enum value -> Random.Generator value
    randomlyChooseOne enum =
        Random.uniform (enum.list |> Stack.top) (enum.list |> Stack.removeTop |> Stack.toList)
    
    example : Enum Order
    example =
        create
            (\lt eq gt order ->
                case order of
                    LT -> lt
                    EQ -> eq
                    GT -> gt
            )
            |> and LT "LT"
            |> and EQ "EQ"
            |> and GT "GT"
    ```
    neat, right?
      - 🧩 [emptiness-typed](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/): `Stack`, `Emptiable`

  - actually store the phantom type
    ```elm
    -- module Quantity exposing (Quantity, Meters, Seconds, ...)
    type Quantity number units
        = Quantity number
    
    type Meters = Meters
    
    meters : number -> Quantity number Meters
    meters = Quantity

    toMeters : Quantity number meters -> number
    toMeters = \(Quantity value) -> value
    ```
    Found the mistake? `meters` needs to be uppercase.
     ```elm
    -- module Quantity exposing (Quantity(..), Meters(..), Seconds(..), ...)
    type Quantity number units
        = In units number
    
    type Meters = Meters
    
    meters : number -> Quantity number units
    meters = In Meters

    inMeters : Quantity number meters -> number
    inMeters =
        -- type mismatch found Meters needs meters
        \(In Meters value) -> value
    ```
    This is also cool because you can easily wrap and unwrap quantities without the need for all those `units, toUnits` for every single unit.
    ```elm
    In : units -> (number -> Quantity number units)
    to : units -> (Quantity number units -> number)
    ```
    (a somewhat similar idea and a bit more is published as [`elm-typed-value`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/))

  - store both the specific value as well as a function to turn it into a more general type
    ```elm
    type Expression
        = Tuple ( Expression, Expression )
        | IntExpression IntExpression
        | Bool Bool
    
    type IntExpression
        = IntDivideBy IntExpression {-//-} IntExpression
        | IntLiteral Int
    
    type alias ExpressionKnown known =
        { known : known
        , toExpression : known -> Expression
        }
    
    tuple :
        ( ExpressionKnown first, ExpressionKnown second )
        -> ExpressionKnown ( first, second )
    tuple = \( first, second ) ->
        { known = ( first.known, second.known )
        , toExpression = \( firstSpecific, secondSpecific ) -> ( firstSpecific |> first.toExpression
            , secondSpecific |> second.toExpression
            )
        }
    
    intLiteral : Int -> ExpressionKnown IntExpression
    intLiteral = \int ->
        { known = int, toExpression = IntLiteral }

    intDivideBy :
        ExpressionKnown IntExpression
        -> (ExpressionKnown IntExpression
            -> ExpressionKnown IntExpression
           )
    intDivideBy divisor = \toDivide ->
        { known = toDivide.known |> IntDivideBy divisor.known
        , toExpression = IntExpression
        }
    ```
    From experience, this only really works when there's a clear "known" and "general" layer.
    E.g. Once you feel like you have to do
    ```elm
    KnownOrGeneral IntLiteral
        (KnownOrGeneral IntExpression
            (KnownOrGeneral NumberExpression
                (KnownOrGeneral ComparableExpression
                    Expression
                )
            )
        )
    ```
    you are doomed.
    Just keeping only the "known" part and letting users explicitly convert between the types will make things a bit noisier but usually that's fine.

With this many alternatives,
are you up for the challenge to try and design your API without phantom types?

## not convinced?

I'm super interested in what you're brewing!
It's not like I haven't used phantom types for [experimental packages](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/) as well.
If you want to, text me @lue on slack.

## performance note

Checking for phantom types in types that expand a lot can get expensive.
I suggest trying it out in watch mode and if it feels slow with your app,
please [open an issue](https://github.com/lue-bird/elm-review-phantom-type/issues). I'm sure we can find edges to optimize.

## thanks
- [jfmengels for the issue: "Follow the trail of phantom types"](https://github.com/jfmengels/elm-review-unused/issues/4)
- [miniBill for elm-fast-dict](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest)
