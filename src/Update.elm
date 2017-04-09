module Update exposing
    ( done

    , Chain

    , andThen
    , andThenIfUnhandled


    , map
    , mapUnhandled

    , mapModel, mapHandledModel

    , andThen2

    , andThenMaybe

    , unhandled, error, handled
    , fromResult
--    , toUnhandled, toError
--    , handledWith


    )
{-| Describe me please...
|-}


type Chain x msg model
    = Unhandled msg model
    | Error x
    | Handled msg (model, Cmd msg)


--type alias Chain x msg model =
--    { msg: msg
--    , model: model
--    , kind: Kind x msg
--    }


-- CONSTRUCTION

unhandled : msg -> model -> Chain x msg model
unhandled msg model = Unhandled msg model

error : x -> Chain x msg model
error x = Error x

handled : msg -> (model, Cmd msg) -> Chain x msg model
handled msg v = Handled msg v


fromResult : msg -> Result x (model, Cmd msg) -> Chain x msg model
fromResult msg r =
    case r of
        Err err -> Error err
        Ok v -> Handled msg v

-- CONVERSION


--toError : x -> Chain x msg model -> Chain x msg model
--toError err c = { c | kind = Error err }


--handledWith : (model, Cmd msg) -> Chain x msg model -> Chain x msg model
--handledWith (model,cmd) c =
--    case c of
--        Handled (mdl, cmd) ->  { c | model = model, kind = Handled <| Cmd.batch [ccmd, cmd] }
--        _ -> { c | model = model, kind = Handled cmd }


--toUnhandled : Chain x msg model -> Chain x msg model
--toUnhandled c = { c | kind = Unhandled }



done : (x ->  (model, Cmd msg)) -> Chain x msg model -> (model, Cmd msg)
done onError c =
    case c of
        Unhandled msg model -> (model, Cmd.none)
        Error x -> onError x
        Handled msg result -> result


concatAfter : Chain x m a -> Chain x m b -> Chain x m b
concatAfter c cc =
    case (c, cc) of
        (Handled  _ (_, cmdA), Handled msg (model, cmdB)) ->
            Handled msg (model, Cmd.batch [cmdA, cmdB])
        _ -> cc



--andThenIf
--    :  (Chain x m a -> Bool)
--    -> (m -> a -> Chain x n b)
--    -> Chain x m a
--    -> Chain x n b
--andThenIf pred update c =
--    if pred c
--        then concatAfter c <| update c.msg c.model
--        else c
----let
----
----    in case c.kind of
----        Error _ -> c
----        _ ->  concatCmds c <| fn c.msg c.model

--notError : Chain x a b -> Bool
--notError c =
--    case c.kind of
--        Error _-> False
--        _ -> True

andThen : (msg -> a -> Chain x msg b) -> Chain x msg a -> Chain x msg b
andThen fn c = -- andThenIf notError fn c
    case c of
        Error e -> Error e
        Unhandled msg model -> fn msg model
        Handled msg (model, cmd) -> concatAfter c <| fn msg model


{-| Like andThen(), but only calls fn if the update is handled
-}
andThenIfHandled : (msg -> a -> Chain x msg a) -> Chain x msg a -> Chain x msg a
andThenIfHandled fn c =
    case c of
        Handled msg (model, cmd) ->  fn msg model
        _ -> c


{-| Like andThen(), but only calls fn if the update is not yet handled
-}
andThenIfUnhandled : (msg -> a -> Chain x msg a) -> Chain x msg a -> Chain x msg a
andThenIfUnhandled fn c =
    case c of
        Unhandled msg model -> fn msg model
        _ -> c


{-| Run an update than may or may not handle the event
-}
andThenMaybe : (m -> a -> Maybe (a, Cmd m)) -> Chain x m a -> Chain x m a
andThenMaybe fn c =
    let
        handler msg model =
            fn msg model
                |> Maybe.map (handled msg)
                |> Maybe.withDefault (unhandled msg model)
    in
        andThen handler c



-- MAP





map : (msg -> a -> (b, Cmd msg)) -> Chain x msg a -> Chain x msg b
map fn c =
    andThen (\msg model -> handled msg <| fn msg model) c



mapUnhandled : (m -> a -> (a, Cmd m)) -> Chain x m a -> Chain x m a
mapUnhandled fn c =
    andThenIfUnhandled (\m a -> handled m <| fn m a ) c


-- MISC





andThen2
    : ((msg,model) -> (msg, model) -> Chain x msg model)
    -> Chain x msg model -> Chain x msg model -> Chain x msg model
andThen2 fn a b =
    case (a,b) of
        (Error _, _) -> a
        (_, Error _) -> b
        _ -> andThen (\ma oa -> andThen (\mb ob -> fn (ma, oa) (mb, ob)) b)  a



-- PARTIAL OPERATIONS




mapModel : (a -> b) -> Chain x msg a -> Chain x msg b
mapModel fn c =
    case c of
        Error err -> Error err
        Unhandled msg model -> Unhandled msg (fn model)
        Handled msg (model, cmd) -> Handled msg (fn model, cmd)

mapHandledModel : (a -> a) -> Chain x msg a -> Chain x msg a
mapHandledModel fn c =
    case c of
        Handled msg (model, cmd) -> Handled msg (fn model, cmd)
        _ -> c


