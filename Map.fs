module App.Map

let inline singleton key value: Map<'Key, 'T> = Map.empty |> Map.add key value

let inverse (map: Map<'Key, 'T>): Map<'T, 'Key> =
    if Map.isEmpty map
    then Map.empty
    else (Map.empty, map) ||> Map.fold (fun inverseMap key value -> Map.add value key inverseMap)
