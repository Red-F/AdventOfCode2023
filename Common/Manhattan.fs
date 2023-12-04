module Common.Manhattan

let distance (sx: int64, sy: int64) (bx: int64, by: int64) = abs (sx - bx) + abs (sy - by)
