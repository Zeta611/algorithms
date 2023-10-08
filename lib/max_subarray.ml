open Base

exception Empty_array

let max_subarray_brute array =
  let len = Array.length array in
  match len with
  | 0 -> raise Empty_array
  | 1 -> (0, 0, array.(0))
  | _ ->
      (* brute force *)
      let i_max = ref 0 in
      let j_max = ref 0 in
      let max = ref array.(0) in

      let sum = ref 0 in
      for i = 0 to len - 1 do
        sum := 0;
        for j = i to len - 1 do
          sum := !sum + array.(j);
          if !sum > !max then (
            i_max := i;
            j_max := j;
            max := !sum)
        done
      done;

      (!i_max, !j_max, !max)

let max_subarray_divide_conquer array =
  let crossing low mid high =
    let sum = ref 0 in
    let left_max = ref Int.min_value in
    let i_max = ref mid in
    for i = mid downto low do
      sum := !sum + array.(i);
      if !sum > !left_max then (
        left_max := !sum;
        i_max := i)
    done;

    sum := 0;
    let right_max = ref Int.min_value in
    let j_max = ref (mid + 1) in
    for j = mid + 1 to high do
      sum := !sum + array.(j);
      if !sum > !right_max then (
        right_max := !sum;
        j_max := j)
    done;

    (!i_max, !j_max, !left_max + !right_max)
  in

  let rec inner low high =
    if low = high then (low, high, array.(low))
    else
      let mid = (low + high) / 2 in
      let left_i, left_j, left_max = inner low mid in
      let right_i, right_j, right_max = inner (mid + 1) high in
      let cross_i, cross_j, cross_max = crossing low mid high in

      if left_max >= right_max && left_max >= cross_max then
        (left_i, left_j, left_max)
      else if right_max >= left_max && right_max >= cross_max then
        (right_i, right_j, right_max)
      else (cross_i, cross_j, cross_max)
  in
  inner 0 (Array.length array - 1)

let max_subarray_linear_rec array =
  let len = Array.length array in
  if len = 0 then raise Empty_array;

  let i_max = ref 0 in
  let j_max = ref 0 in
  let max = ref array.(0) in

  let rec ends_with j =
    if j = 0 then (0, array.(0))
    else
      let i, s = ends_with (j - 1) in
      let s' = s + array.(j) in
      let i_curr, s_curr =
        if array.(j) >= s' then (j, array.(j)) else (i, s')
      in
      if s_curr > !max then (
        i_max := i_curr;
        j_max := j;
        max := s_curr);
      (i_curr, s_curr)
  in

  ends_with (len - 1) |> ignore;

  (!i_max, !j_max, !max)

(** My thought process came up with max_subarray_linear_rec first, then I de-recursified it *)
let max_subarray_linear array =
  let len = Array.length array in
  if len = 0 then raise Empty_array;

  let i_max = ref 0 in
  let j_max = ref 0 in
  let max = ref array.(0) in

  let i_ending_max = ref 0 in
  let s_ending_max = ref array.(0) in
  for j = 1 to len - 1 do
    let s' = !s_ending_max + array.(j) in
    if array.(j) >= s' then (
      i_ending_max := j;
      s_ending_max := array.(j))
    else s_ending_max := s';

    if !s_ending_max > !max then (
      i_max := !i_ending_max;
      j_max := j;
      max := !s_ending_max)
  done;

  (!i_max, !j_max, !max)
