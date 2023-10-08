open Base

type adjacency_list = int list
type graph = adjacency_list array
type color = White | Gray | Black

type attribute = {
  mutable color : color;
  mutable distance : int;
  mutable predecessor : int option;
}

let bfs graph source =
  let attrs =
    Array.init (Array.length graph) ~f:(fun _ ->
        { color = White; distance = Int.max_value; predecessor = None })
  in

  attrs.(source) <- { color = Gray; distance = 0; predecessor = None };
  let queue = Queue.create () in
  Queue.enqueue queue source;

  while not (Queue.is_empty queue) do
    (* invariant: queue is the set of all gray vertices *)
    let u = Queue.dequeue_exn queue in
    (* u is gray *)
    graph.(u)
    |> List.iter ~f:(fun v ->
           (* v is adjacent to u *)
           match attrs.(v).color with
           | White ->
               attrs.(v).color <- Gray;
               attrs.(v).distance <- attrs.(u).distance + 1;
               attrs.(v).predecessor <- Some u;
               Queue.enqueue queue v
           | _ -> ());
    (* every vertices adjacent to u have been visited *)
    attrs.(u).color <- Black
  done;

  attrs
