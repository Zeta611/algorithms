open Base
open Stdio

type algorithm =
  | Max_subarray of [ `Brute | `Divide_conquer | `Linear_rec | `Linear ]

let run algorithm =
  let open Algorithms in
  match algorithm with
  | Max_subarray impl ->
      let f =
        let open Max_subarray in
        match impl with
        | `Brute -> max_subarray_brute
        | `Divide_conquer -> max_subarray_divide_conquer
        | `Linear_rec -> max_subarray_linear_rec
        | `Linear -> max_subarray_linear
      in
      let array =
        [| 13; -3; -25; 20; -3; -16; -23; 18; 20; -7; 12; -5; -22; 15; -4; 7 |]
      in
      let i, j, s = f array in
      printf "i: %d, j: %d, s: %d\n" i j s

let () =
  [ `Brute; `Divide_conquer; `Linear_rec; `Linear ]
  |> List.map ~f:(fun x -> Max_subarray x)
  |> List.iter ~f:run
