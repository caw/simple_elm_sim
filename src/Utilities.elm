module Utilities exposing (ntimes)

-- module Utilities exposing (myLocale, ntimes, printDecimal)
-- import FormatNumber exposing (format)
-- import FormatNumber.Locales exposing (Locale)


ntimes : Int -> (a -> a) -> a -> a
ntimes n func arg =
    case n of
        1 ->
            func arg

        _ ->
            ntimes (n - 1) func (func arg)



-- myLocale : Locale
-- myLocale =
--     { decimals = 2
--     , thousandSeparator = ""
--     , decimalSeparator = "."
--     }
-- {-| NB: this also rounds
-- -}
-- printDecimal : Float -> Int -> String
-- printDecimal n places =
--     let
--         loc =
--             { myLocale
--                 | decimals = places
--             }
--     in
--     format loc n
