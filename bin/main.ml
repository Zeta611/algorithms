open Base
open Stdio

type algorithm =
  | Max_subarray of [ `Brute | `Divide_conquer | `Linear_rec | `Linear ]
  | Bfs

let run algorithm =
  let open Algorithms in
  match algorithm with
  | Max_subarray impl ->
      printf "=== Max Subarray ===\n";
      let open Max_subarray in
      let f =
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
  | Bfs ->
      printf "=== BFS ===\n";
      let open Bfs in
      let graph =
        [|
          (* 0 *) [ 1; 4 ];
          (* 1 *) [ 0; 5 ];
          (* 2 *) [ 3; 5; 6 ];
          (* 3 *) [ 2; 6; 7 ];
          (* 4 *) [ 0 ];
          (* 5 *) [ 1; 2; 6 ];
          (* 6 *) [ 2; 3; 5; 7 ];
          (* 7 *) [ 3; 6 ];
        |]
      in
      let attrs = bfs graph 1 in
      Array.iteri attrs ~f:(fun i attr ->
          printf "v_%d: d = %d; p = v_%d\n" i attr.distance
            (attr.predecessor |> Option.value ~default:(-1)))

let () =
  ([ `Brute; `Divide_conquer; `Linear_rec; `Linear ]
  |> List.map ~f:(fun x -> Max_subarray x))
  @ [ Bfs ]
  |> List.iter ~f:run
