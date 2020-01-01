module Bimap

open App
open System.Collections.Generic

type Bimap<'Key, 'Value when 'Key: comparison and 'Value: comparison> private (map: Map<'Key, 'Value>, inverseMap: Map<'Value, 'Key>) =

    /// The empty Bimap instance.
    static let empty = Bimap(Map.empty, Map.empty)

    static member Empty = empty

    member _.ForwardMap
        with private get () = map

    member _.InverseMap
        with private get () = inverseMap

    static member private Equals(l: Bimap<'Key, 'Value>,
                                 r: Bimap<'Key, 'Value>) =
        l.ForwardMap = r.ForwardMap
        && l.InverseMap = r.InverseMap

    static member private Compare(l: Bimap<'Key, 'Value>,
                                  r: Bimap<'Key, 'Value>) =
        match compare l.ForwardMap r.ForwardMap with
        | 0 -> compare l.InverseMap r.InverseMap
        | x -> x


    override this.Equals other =
        match other with
        | :? Bimap<'Key, 'Value> as other ->
            Bimap<_, _>.Equals(this, other)
        | _ -> false

    override _.GetHashCode() = map.GetHashCode()

    member _.Count = Map.count map

    member _.IsEmpty = Map.isEmpty map

    member _.ContainsKey key = Map.containsKey key map

    member _.ContainsValue value =
        Map.containsKey value inverseMap

    member _.Find key = Map.find key map

    member _.FindValue value = Map.find value inverseMap

    member _.Paired(key, value) =
        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match Map.tryFind key map with
        | None -> false
        | Some v -> v = value


    member this.Remove key =
        // Use the key to find its corresponding value.
        match Map.tryFind key map with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            Bimap
                (Map.remove key map,
                 Map.remove value inverseMap)


    member this.RemoveValue value =
        // Use the key to find its corresponding value.
        match Map.tryFind value inverseMap with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some key ->
            // Remove the values from both maps.
            Bimap
                (Map.remove key map,
                 Map.remove value inverseMap)

    member _.TryFind key = Map.tryFind key map

    member _.TryFindValue value =
        Map.tryFind value inverseMap

    static member Singleton(key, value): Bimap<'Key, 'Value> =
        Bimap
            (Map.singleton key value,
             Map.singleton value key)

    member this.Add key value =
        // Add the values to both maps.
        // As in Map, we overwrite any existing entry; however, we have to be
        // a bit more thorough here to ensure the invariant is maintained.
        // OPTIMIZE : This could be implemented more efficiently to remove
        // unnecessary or duplicated checks while still maintaining the invariant.
        // It'd also be nice if we could do this in a way that detects if the values
        // are already present and bound to each other, so we don't need to alter the Bimap at all...
        // TODO : Create a private "AddUnsafe" method to avoid the lookups in TryAdd
        this.Remove(key).RemoveValue(value)
            .TryAdd(key, value)

    member this.TryAdd(key, value) =
        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match Map.tryFind key map,
              Map.tryFind value inverseMap with
        | None, None ->
            Bimap
                (Map.add key value map,
                 Map.add value key inverseMap)

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            this

    member __.Iterate(action: 'Key -> 'Value -> unit): unit =
        Map.iter action map

    member __.Fold(folder: 'State -> 'Key -> 'Value -> 'State,
                   state: 'State): 'State =
        Map.fold folder state map

    member __.FoldBack(folder: 'Key -> 'Value -> 'State -> 'State,
                       state: 'State): 'State =
        Map.foldBack folder map state

    member this.Filter(predicate: 'Key -> 'Value -> bool) =
        let folder (bimap: Bimap<_, _>) key value =
            if predicate key value then
                bimap
            else
                bimap.Remove key

        this.Fold(folder, this)


    member this.Partition(predicate: 'Key -> 'Value -> bool) =
        // Partition efficiently by removing elements from the original map
        // and adding them to a new map when the predicate returns false
        // (instead of creating two new maps).
        this.Fold
            ((fun (trueBimap: Bimap<_, _>, falseBimap: Bimap<_, _>) key value ->
                if predicate key value then
                    trueBimap, falseBimap
                else
                    trueBimap.Remove key,
                    (falseBimap.Add key value)),

             (this, empty))

    static member OfSeq sequence: Bimap<'Key, 'Value> =
        // Preconditions
        //checkNonNull "sequence" sequence
        (empty, sequence)
        ||> Seq.fold
                (fun bimap (key, value) ->
                    bimap.Add key value)

    static member OfList list: Bimap<'Key, 'Value> =
        // Preconditions
        // checkNonNull "list" list

        (empty, list)
        ||> List.fold
                (fun bimap (key, value) ->
                    bimap.Add key value)

    static member OfArray array: Bimap<'Key, 'Value> =
        // Preconditions
        // checkNonNull "array" array
        (empty, array)
        ||> Array.fold
                (fun bimap (key, value) ->
                    bimap.Add key value)

    static member OfMap map: Bimap<'Key, 'Value> =
        // Preconditions
        // checkNonNull "map" map

        /// The inverse map.
        let inverseMap = Map.inverse map

        // Remove any bindings from the original map which don't exist in the inverse map.
        let map =
            (map, map)
            ||> Map.fold (fun map k v ->
                    match Map.tryFind v inverseMap with
                    | None -> Map.remove k map
                    | Some k' ->
                        if k = k' then
                            map
                        else
                            Map.remove k map)

        Bimap(map, Map.inverse map)


    member _.ToSeq(): seq<'Key * 'Value> = Map.toSeq map
    member _.ToList(): ('Key * 'Value) list =
        Map.toList map
    member _.ToArray(): ('Key * 'Value) [] =
        Map.toArray map
    member _.ToMap(): Map<'Key, 'Value> = map

    member internal this.LeftKvpArray(): KeyValuePair<'Key, 'Value> [] =
        let elements = ResizeArray(1024)
        this.Iterate
        <| fun key value ->
            elements.Add(KeyValuePair(key, value))

        elements.ToArray()

    member internal this.RightKvpArray(): KeyValuePair<'Value, 'Key> [] =
        let elements = ResizeArray(1024)
        this.Iterate
        <| fun key value ->
            elements.Add(KeyValuePair(value, key))

        elements.ToArray()

    interface System.IEquatable<Bimap<'Key, 'Value>> with
        member this.Equals other =
            Bimap<_, _>.Equals(this, other)

    interface System.IComparable<Bimap<'Key, 'Value>> with
        member this.CompareTo other =
            Bimap<_, _>.Compare(this, other)
