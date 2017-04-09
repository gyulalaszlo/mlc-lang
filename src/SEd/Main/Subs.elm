module SEd.Main.Subs exposing (..)
{-| Describe me please...
-}

import Keyboard
import SEd.Model exposing (Model, Msg(KeyPress))

subscriptions : Model x s c n -> Sub Msg s c n
subscriptions model =
    Sub.batch
        [ Keyboard.presses KeyPress
--        , Keyboard.downs KeyDown
--        , Keyboard.ups KeyUp
        ]

