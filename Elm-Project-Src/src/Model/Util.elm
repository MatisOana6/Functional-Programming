module Model.Util exposing (..)

{-| Chains two `Order`s. It first checks the second parameter:

  - if it's `EQ`, then it returns the first parameter
  - otherise it returns it as is.

Meant to be used with the pipeline operator:

-}


chainCompare : Order -> Order -> Order
chainCompare ord2 ord1 =
    case ord1 of
        EQ ->
            ord2

        other ->
            other
