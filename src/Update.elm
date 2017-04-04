module Update exposing
    ( done

    , Chain
    , andThen, map

    , andThen2

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

map : (msg -> a -> (b, Cmd msg)) -> Chain x msg a -> Chain x msg b
map fn c =
    andThen (\msg model -> handled msg <| fn msg model) c


andThen2
    : ((msg,model) -> (msg, model) -> Chain x msg model)
    -> Chain x msg model -> Chain x msg model -> Chain x msg model
andThen2 fn a b =
    case (a,b) of
        (Error _, _) -> a
        (_, Error _) -> b
        _ -> andThen (\ma oa -> andThen (\mb ob -> fn (ma, oa) (mb, ob)) b)  a
        --a fn (ma,oa) (mb,ob)
--        (Handled ma (oa, ca), Unhandled mb ob) -> fn (ma,oa) (mb,ob)

{-

    Update.unhandled msg model
        |> Update.andThen (updateStateMachine)
        |> Update.finish (\err model -> {model | error = err } ! [])


-}