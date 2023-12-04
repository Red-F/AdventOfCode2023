module Common.Matrix

open System.Collections.Generic

type sparseMatrix() =
  let table = Dictionary<int64 * int64, char>()
  // let table = Dictionary<(int64 * int64), char>()
  member _.Item
    // Because the key is comprised of two values, 'get' has two index values
    with get(key1, key2) = table[(key1, key2)]
    // 'set' has two index values and a new value to place in the key's position
    and set (key1, key2) value = table[(key1, key2)] <- value
  member this.Keys =
    table.Keys
  member this.ContainsKey x =
    table.ContainsKey x
