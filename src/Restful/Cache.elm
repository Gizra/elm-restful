module Restful.Cache exposing
    ( Cache, ReadOnlyCache, empty, withPageSize
    , getParams, setParams, mapParams
    , invalidate
    , Msg, fetchFrom, tryFetching
    , update
    , Values(..), Ranges, getValues, loaded, allItemsLoaded
    , FetchRequest, getFetchRequests, getFetchErrors, forgetFetchErrors
    )

{-| Given an endpoint, manage data retrieved from that endpoint.

At the moment, this is read-only, because that's all that was needed for the
app this was extracted from. This should handle writing and validating at some
point, of course.

@docs Cache, ReadOnlyCache, empty, withPageSize

@docs getParams, setParams, mapParams
@docs invalidate

@docs Msg, fetchFrom, tryFetching

@docs update

@docs Values, Ranges, getValues, loaded, allItemsLoaded
@docs FetchRequest, getFetchRequests, getFetchErrors, forgetFetchErrors

-}

import AssocList as Dict exposing (Dict)
import IntDict exposing (IntDict)
import List
import Restful.Endpoint exposing (AccessToken, BackendUrl, CrudRequest, EndPoint, QueryResult, ReadOnly, selectRange, toCmd, withAccessToken)


{-| Represents a local cache of values from an endpoint.

Note that, at least for now, we're assuming that the `params` type has reliable
behaviour for `==` (which the compiler doesn't guarantee) ... we could fix that
by requiring a comparison function at some point.

-}
type Cache w e k v c p
    = Cache (CacheRec w e k v c p)


{-| An alias for a `Cache` which works with a read-only endpoint.
-}
type alias ReadOnlyCache e k v p =
    Cache ReadOnly e k v v p


type alias CacheRec w e k v c p =
    { endpoint : EndPoint w e k v c p
    , values : Values k v p
    , fetchRequests : IntDict (FetchRequest p)
    , fetchErrors : List ( FetchRequest p, e )
    , nextRequestId : Int
    , pageSize : Maybe Int
    }


{-| What have we received from the backend?
-}
getValues : Cache w e k v c p -> Values k v p
getValues (Cache { values }) =
    values


{-| What fetch requests are in-progress?
-}
getFetchRequests : Cache w e k v c p -> List (FetchRequest p)
getFetchRequests (Cache { fetchRequests }) =
    IntDict.values fetchRequests


{-| What fetch errors have occured since the last successful fetch?
-}
getFetchErrors : Cache w e k v c p -> List ( FetchRequest p, e )
getFetchErrors (Cache { fetchErrors }) =
    fetchErrors


{-| Forget all fetch errors ... for instance, if the user has acknowledged them
and you don't want to show them any longer.

Once you've done this, `tryFetching` will actually try fetching again. So, it's
the basic mechanism for a kind of "retry" after an error. (We don't want to
automatically retry after an error, since that could well produce an infinite
loop of errors and retries).

-}
forgetFetchErrors : Cache w e k v c p -> Cache w e k v c p
forgetFetchErrors (Cache cacheRec) =
    Cache { cacheRec | fetchErrors = [] }


{-| Represents the items we've received from the backend.
-}
type Values k v p
    = -- We're waiting for the first response, using these params.
      -- The second parameter is the previous values we had, if any,
      -- now either invalidated or with different params.
      Pending p (Maybe (Ranges k v p))
      -- At least some values have arrived.
    | Arrived (Ranges k v p)


emptyValues : p -> Values k v p
emptyValues params =
    Pending params Nothing


{-| Represents values we've received from the backend.

  - `items` is a dictionary of items we've received from the backend. The key
    to the outer dictionary is the offset into the order of the items. The idea
    is that we can represent a bunch of items from the "middle" without needing
    a placeholder for every value. (So, ideally, to support possibly showing
    just a few items out of millions).

  - `count` is how many items the backend says it has in total.

  - `params` are the params we used to fetch these items.

-}
type alias Ranges k v p =
    { items : IntDict (Dict k v)
    , count : Int
    , params : p
    }


{-| How many values have we actually loaded from the backend?
-}
loaded : Ranges k v p -> Int
loaded ranges =
    let
        addSizes _ dict accum =
            accum + Dict.size dict
    in
    IntDict.foldl addSizes 0 ranges.items


{-| Have we loaded all the items?
-}
allItemsLoaded : Ranges k v p -> Bool
allItemsLoaded ranges =
    ranges.count == loaded ranges


{-| Encapsulates information about a fetch request.
-}
type alias FetchRequest p =
    { params : p
    , offset : Int
    , pageSize : Maybe Int
    }


{-| Given an endpoint, create a cache for values to be send to/from
that endpoint.

For now, at least, the params are required ... you can arrange the
EndPoint to have no params by using ().

-}
empty : p -> EndPoint w e k v c p -> Cache w e k v c p
empty params endpoint =
    -- Eventually, caches created from the same endpoint ought to
    -- intercommunicate a bit. The EndPoint is (so far) meant to be pretty
    -- static, so you could imagine a `CacheManager` to tie things together.
    -- Which could be per-endpoint.
    --
    -- The use case for caches created from the same enpoint would be cases
    -- where you might have two widgets on the page at once, each showing
    -- data from the same endpoint, but applying different params or sort
    -- order etc. So, they don't necessarily have identical contents, but
    -- ought to communicate a bit (when new things are added etc.).
    Cache
        { endpoint = endpoint
        , values = emptyValues params
        , fetchErrors = []
        , fetchRequests = IntDict.empty
        , nextRequestId = 0
        , pageSize = Nothing
        }


{-| Use the specificed page size when contacting the backend. By default,
we don't speficy a page size. However, the backend may paginate anyway,
of course.
-}
withPageSize : Int -> Cache w e k v c p -> Cache w e k v c p
withPageSize pageSize (Cache cache) =
    Cache { cache | pageSize = Just pageSize }


{-| Messages we can handle. Needed for things that may kick off contact with
the backend.
-}
type Msg e k v p
    = FetchFrom Int
    | HandleFetched Int (Result e (QueryResult k v p))


{-| Get things from the backend at the specific offset. Uses the page size
specified for the cache, if any. (Note that the server won't necessarily
return the page size you want).

This unconditionally contacts the backend, even if we already have values
cached.

-}
fetchFrom : Int -> Msg e k v p
fetchFrom =
    FetchFrom


{-| Given an offset and a number of items to check from that offset onwards,
fetch the next pageSize that we don't have.

This takes into account:

  - data we already have
  - requests that are in-flight
  - errors we have received

That is, it doesn't automatically retry errors.

If the number to fetch is unspecified, we try to load all data from the offset.

-}
tryFetching : Int -> Maybe Int -> Cache w e k v c p -> Maybe (Msg e k v p)
tryFetching offset range (Cache { values, fetchRequests, fetchErrors }) =
    if IntDict.isEmpty fetchRequests then
        if List.isEmpty fetchErrors then
            case values of
                Pending _ _ ->
                    -- We're pending, so no actual values in hand. For now,
                    -- just fetch one page at a time. Eventually, we could send
                    -- multiple requests at once, with different offsets.
                    Just <| FetchFrom offset

                Arrived ranges ->
                    let
                        -- We know the total count, so see what the max range is from
                        -- this offset.
                        maxRange =
                            ranges.count - offset

                        -- Our adjusted range is either the lesser of the requested
                        -- range and the max range, or, if no range requested, the
                        -- max range.
                        adjustedRange =
                            range
                                |> Maybe.map (min maxRange)
                                |> Maybe.withDefault maxRange
                    in
                    -- We have data, so what we want to figure out is whether
                    -- our data covers the offset and range we desire. So, we
                    -- fold through the data, reducing the requested offset and
                    -- range, possibly to nothing if we have it all.
                    ranges.items
                        |> IntDict.foldl applyItems { offset = offset, range = adjustedRange }
                        |> (\result ->
                                if result.range > 0 then
                                    Just <| FetchFrom result.offset

                                else
                                    Nothing
                           )

        else
            -- For now, just bail if there has been any error. Eventually,
            -- we should be more sophisticated about this, and see whether
            -- the error actually covers the requested range. Or, perhaps not ...
            -- I suppose it's unlikely that fetch errors will be range-dependent?
            Nothing

    else
        -- For now, only let one request be in-flight at a time. Eventually, we
        -- should be more sophisticated than this. Basically, the right
        -- question is: do we have any fetch requests that would answer the
        -- offset & range? If so, we should cut down the offset and range on
        -- the assumption that they will succeed ... possibly to nothing.
        Nothing


applyItems : Int -> Dict k v -> { offset : Int, range : Int } -> { offset : Int, range : Int }
applyItems index list accum =
    if accum.range > 0 then
        if index > accum.offset then
            -- We're past the offset we want. So, the only question is whether
            -- the range would extend past our offset ... if so, cut it down so
            -- that it doesn't.
            { accum | range = min accum.range (index - accum.offset) }

        else
            -- The data is at or to the left of our offset. So, the question is
            -- the extent of the overlap, if any.
            let
                overlap =
                    index + Dict.size list - accum.offset
            in
            if overlap > 0 then
                { offset = accum.offset + overlap
                , range = accum.range - overlap
                }

            else
                accum

    else
        -- If we've reduced the range to zero, then we don't
        -- need to think any longer
        accum


{-| Set the params to apply. If they have changed, the cache will be
invalidated.

We don't automatically trigger a fetch when you set params, since who knows how
you'll want to start filling the cache. So, you should have code that checks
whether the offsets you need are present ... invalidating the cache will then
affect that (since they won't be).

-}
setParams : p -> Cache w e k v c p -> Cache w e k v c p
setParams params ((Cache cacheRec) as cache) =
    case cacheRec.values of
        Pending oldParams oldRanges ->
            if params == oldParams then
                -- params not changed, so do nothing
                cache

            else
                -- Just subsitute the new params for the old, keeping
                -- the old ranges if we have any.
                Cache { cacheRec | values = Pending params oldRanges }

        Arrived oldRanges ->
            if params == oldRanges.params then
                -- params not changed, so do nothing
                cache

            else
                -- Use the new params, and save the old ranges
                Cache { cacheRec | values = Pending params (Just oldRanges) }


{-| What are our currently active params?
-}
getParams : Cache w e k v c p -> p
getParams (Cache { values }) =
    paramsFromValues values


{-| Given a function which updates params, update the `Cache`
-}
mapParams : (p -> p) -> Cache w e k v c p -> Cache w e k v c p
mapParams func cache =
    cache
        |> getParams
        |> func
        |> (\a -> setParams a cache)


paramsFromValues : Values k v p -> p
paramsFromValues values =
    case values of
        Pending params _ ->
            params

        Arrived ranges ->
            ranges.params


{-| Throw away the current values. This doesn't, by itself, trigger a new
fetch. Also, it doesn't invalidate fetches in-flight ... if results are
received after this, from fetches initiated before this, they will still be
used. (Perhaps a variant that also invalidates fetches in flight would be
useful?)
-}
invalidate : Cache w e k v c p -> Cache w e k v c p
invalidate ((Cache cacheRec) as cache) =
    case cacheRec.values of
        Pending _ _ ->
            -- We had no data, so nothing to do.
            cache

        Arrived oldRanges ->
            -- Switch back to pending, but save the ranges
            Cache
                { cacheRec | values = Pending oldRanges.params (Just oldRanges) }


applyAccessToken : Maybe AccessToken -> CrudRequest a b -> CrudRequest a b
applyAccessToken accessToken =
    case accessToken of
        Just token ->
            withAccessToken token

        Nothing ->
            identity


{-| Our update function.
-}
update : BackendUrl -> Maybe AccessToken -> Msg e k v p -> Cache w e k v c p -> ( Cache w e k v c p, Cmd (Msg e k v p) )
update backendUrl accessToken msg ((Cache cacheRec) as cache) =
    case msg of
        FetchFrom offset ->
            let
                params =
                    getParams cache

                request =
                    { params = params
                    , offset = offset
                    , pageSize = cacheRec.pageSize
                    }
            in
            ( Cache
                { cacheRec
                    | nextRequestId = cacheRec.nextRequestId + 1
                    , fetchRequests = IntDict.insert cacheRec.nextRequestId request cacheRec.fetchRequests
                }
            , selectRange backendUrl cacheRec.endpoint params offset cacheRec.pageSize
                |> applyAccessToken accessToken
                |> toCmd (HandleFetched cacheRec.nextRequestId)
            )

        HandleFetched requestId response ->
            case IntDict.get requestId cacheRec.fetchRequests of
                Just request ->
                    let
                        fetchRequests =
                            IntDict.remove requestId cacheRec.fetchRequests
                    in
                    if request.params == paramsFromValues cacheRec.values then
                        -- These are the params we're still looking for
                        ( Cache
                            { cacheRec
                                | fetchRequests = fetchRequests
                                , fetchErrors = updateFetchErrors request response cacheRec.fetchErrors
                                , values = updateValues response cacheRec.values
                            }
                        , Cmd.none
                        )

                    else
                        -- If the params don't match, just ignore the response.
                        ( Cache { cacheRec | fetchRequests = fetchRequests }
                        , Cmd.none
                        )

                Nothing ->
                    -- If we've forgotten this request, it's because we've pre-determined that
                    -- we don't want its results anymore. So, do nothing.
                    ( cache
                    , Cmd.none
                    )


{-| Given a response from the backend, how should we update our values?
-}
updateValues : Result e (QueryResult k v p) -> Values k v p -> Values k v p
updateValues response values =
    case response of
        Err _ ->
            -- We don't change the values at all on an error
            values

        Ok query ->
            case values of
                Pending params _ ->
                    -- We have our first response
                    initialRange query

                Arrived data ->
                    if query.count == data.count then
                        -- The count is the same, so integrate
                        Arrived { data | items = integrateQuery query data.items }

                    else
                        -- The count has changed, so we can't really trust any
                        -- of our old data ... just keep the new stuff.
                        initialRange query


{-| Given some data from the backend, update our local data.

Note that we don't assume that we're getting the same pageSize every time, or
that our offsets are aligned.

-}
integrateQuery : QueryResult k v p -> IntDict (Dict k v) -> IntDict (Dict k v)
integrateQuery query ranges =
    -- So far, we're not consolidating ranges here ... that may or may not be a
    -- good idea at some point. Ultimately, a number of optimizations are likely
    -- to be possible here.
    let
        go offset values ( remaining, accum ) =
            if List.isEmpty remaining.items then
                -- If we don't have more items to integrate, then just add this
                -- one back in.
                ( remaining, IntDict.insert offset values accum )

            else
                let
                    sizeRemaining =
                        List.length remaining.items

                    sizeOfTheseValues =
                        Dict.size values

                    offsetIntoTheseValues =
                        remaining.offset - offset

                    replaceInTheseValues =
                        min sizeRemaining (sizeOfTheseValues - offsetIntoTheseValues)
                in
                if replaceInTheseValues <= 0 then
                    -- Nothing to replace here, so just add it back in
                    ( remaining, IntDict.insert offset values accum )

                else
                    let
                        newValues =
                            Dict.concat
                                [ Dict.take offsetIntoTheseValues values
                                , Dict.fromList <| List.take replaceInTheseValues remaining.items
                                , Dict.drop (offsetIntoTheseValues + replaceInTheseValues) values
                                ]
                    in
                    ( { remaining
                        | offset = remaining.offset + replaceInTheseValues
                        , items = List.drop replaceInTheseValues remaining.items
                      }
                    , IntDict.insert offset newValues accum
                    )
    in
    IntDict.foldl go ( query, IntDict.empty ) ranges
        |> (\( remaining, accum ) ->
                if List.isEmpty remaining.items then
                    accum

                else
                    IntDict.insert remaining.offset (Dict.fromList remaining.items) accum
           )


{-| A helper for constructing an `Arrived` value from an initial range.
-}
initialRange : QueryResult k v p -> Values k v p
initialRange result =
    Arrived
        { items = IntDict.singleton result.offset (Dict.fromList result.items)
        , count = result.count
        , params = result.params
        }


{-| Given a response, update fetch errors.
-}
updateFetchErrors : FetchRequest p -> Result e a -> List ( FetchRequest p, e ) -> List ( FetchRequest p, e )
updateFetchErrors request result errors =
    case result of
        Err err ->
            -- Add the error to the front
            ( request, err ) :: errors

        Ok _ ->
            -- If we get a successful fetch, we forget all fetch errors.
            []
