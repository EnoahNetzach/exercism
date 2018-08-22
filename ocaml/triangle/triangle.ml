open Base

let is_triangle a b c =
  let sorted = List.nth_exn (List.sort ~compare:Int.compare [a; b; c]) in
  let (a, b, c) = (sorted 0, sorted 1, sorted 2) in
  c > 0 && c <= a + b

let is_equilateral a b c = is_triangle a b c && a = b && b = c 

let is_isosceles a b c = is_triangle a b c && (a = b || a = c || b = c)

let is_scalene a b c = is_triangle a b c && (a <> b && b <> c)

