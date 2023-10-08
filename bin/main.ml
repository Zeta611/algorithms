open Stdio
open Algorithms.Max_subarray

let array =
  [| 13; -3; -25; 20; -3; -16; -23; 18; 20; -7; 12; -5; -22; 15; -4; 7 |]

let () =
  let i, j, s = max_subarray_brute array in
  printf "i: %d, j: %d, s: %d\n" i j s;

  let i, j, s = max_subarray_divide_conquer array in
  printf "i: %d, j: %d, s: %d\n" i j s;

  let i, j, s = max_subarray_linear_rec array in
  printf "i: %d, j: %d, s: %d\n" i j s;

  let i, j, s = max_subarray_linear array in
  printf "i: %d, j: %d, s: %d\n" i j s
