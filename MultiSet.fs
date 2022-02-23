module MultiSet

type MultiSet<'a> when 'a: comparison = Map<'a, uint32>

let empty : MultiSet<'a> = Map.empty

let isEmpty (s:MultiSet<'a>) = Map.isEmpty s

let size (s:MultiSet<'a>) : uint32 = Map.fold (fun acc _ value -> acc + value) 0u s

let contains a (s:MultiSet<'a>) : bool = Map.containsKey a s

let numItems a (s:MultiSet<'a>) : uint32 = if s.ContainsKey a then s.[a] else 0u

let add key value (s:MultiSet<'a>):MultiSet<'a> = Map.add key ((numItems key s) + value) s

let addSingle key (s:MultiSet<'a>) = Map.add key ((numItems key s) + 1u) s

let remove (key:'a) (n:uint32) (s:MultiSet<'a>) =
    match key, n, s with
    | key,n,s when (numItems key s) < n -> Map.remove key s
    | _ -> Map.add key ((numItems key s) - n) s

let removeSingle (key:'a) (s:MultiSet<'a>) =
    match key, s with
    | key,s when (numItems key s) <= 0u -> Map.remove key s
    | _ -> Map.add key ((numItems key s) - 1u) s

let fold (f:('a -> 'b -> uint32 -> 'a)) (a:'a) (s:MultiSet<'b>) : 'a = Map.fold f a s

let foldBack (f:('a -> uint32 -> 'b -> 'b)) (s:MultiSet<'a>) (b:'b) : 'b = Map.foldBack f s b

let ofList (list:List<'a>) : MultiSet<'a> = List.fold (fun acc a -> (addSingle a acc)) empty list

let toList (s:MultiSet<'a>) : List<'a> = fold (fun acc key value -> List.append acc ) List.Empty s