module type VERTEX =
sig
    type label
    type t

    val equal:  t -> t -> bool
    val create: label -> t
    val label:  t -> label
end;;

module type EDGE =
sig
    type t
    type label
    type vertex

    val equal:  t -> t -> bool
    val create: label -> vertex -> vertex -> t
    val label:  t -> label
    val start_: t -> vertex
    val end_:   t -> vertex
end;;

module type GRAPH =
sig
    type t

    module V: VERTEX
    type vertex = V.t

    module E: EDGE with type vertex = vertex
    type edge = E.t

    val mem_v:      t -> vertex -> bool
    val mem_e:      t -> edge -> bool
    val mem_e_v:    t -> vertex -> vertex -> bool
    val find_e:     t -> vertex -> vertex -> edge
    val succ:       t -> vertex -> vertex list
    val pred:       t -> vertex -> vertex list
    val succ_e:     t -> vertex -> edge list
    val pred_e:     t -> vertex -> edge list

    val empty:  t
    val add_e:  t -> edge -> t
    val add_v:  t -> vertex -> t
    val rem_e:  t -> edge -> t
    val rem_v:  t -> vertex -> t

    val fold_v: (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_e: (edge -> 'a -> 'a) -> t -> 'a -> 'a

    val dfs: t -> vertex -> vertex list
    val bfs: t -> vertex -> vertex list
end;;

module Vertex : VERTEX with type label = int=
struct
    type label  = int
    type t      = Vertex of label

    let equal (Vertex (label1)) (Vertex (label2))   = label1 = label2;;
    let create label                                = Vertex (label);;
    let label (Vertex (label))                      = label;;
end;;

module Edge (Vertex: VERTEX) : EDGE with type label = int and type vertex = Vertex.t =
struct
    type label  = int
    type vertex = Vertex.t
    type t      = Edge of label * vertex * vertex

    let equal (Edge (label1, s1, e1)) (Edge (label2, s2, e2))   =   label1 = label2
                                                                &&  Vertex.equal s1 s2
                                                                &&  Vertex.equal e1 e2;;
    let create label s e            = Edge (label, s, e);;
    let label (Edge (label, _, _))  = label;;
    let start_ (Edge (_, s, _))     = s;;
    let end_ (Edge (_, _, e))       = e;;
end;;

module Graph (Vertex: VERTEX) (Edge: EDGE with type label = int and type vertex = Vertex.t) : GRAPH with module V = Vertex and module E = Edge =
struct
    module V    = Vertex
    module E    = Edge

    type vertex = V.t
    type edge   = E.t
    type t      = Graph of vertex list * edge list

    let mem_v (Graph (vertices, _)) v   = List.mem v vertices;;
    let mem_e (Graph (_, edges)) e      = List.mem e edges;;
    let mem_e_v (Graph (_, edges)) s e  = List.exists (fun edge -> E.start_ edge = s && E.end_ edge = e) edges;;
    let find_e (Graph (_, edges)) s e   = List.find (fun edge -> E.start_ edge = s && E.end_ edge = e) edges;;

    let succ (Graph (vertices, _) as g) v   = List.filter (mem_e_v g v) vertices;;
    let pred (Graph (vertices, _) as g) v   = List.filter (fun s -> mem_e_v g s v) vertices;;

    let succ_e (Graph (_, edges)) v = List.filter (fun edge -> E.start_ edge = v) edges;;
    let pred_e (Graph (_, edges)) v = List.filter (fun edge -> E.end_ edge = v) edges;;

    let empty   = Graph ([], []);;

    let add_e (Graph (vertices, edges)) e   = Graph (vertices, e :: edges);;
    let add_v (Graph (vertices, edges)) v   = Graph (v :: vertices, edges);;

    let rem_e (Graph (vertices, edges)) e   = Graph (vertices, List.filter (fun e_ -> e != e_) edges);;
    let rem_v (Graph (vertices, edges)) v   = Graph (List.filter (fun v_ -> v != v_) vertices, edges);;

    let fold_v func (Graph (vertices, _)) b = List.fold_right func vertices b;;
    let fold_e func (Graph (_, edges)) b    = List.fold_right func edges b;;

    let dfs graph start =
        let rec search acc = function
            | []            -> List.rev acc
            | idx :: tail   -> search (idx :: acc) ((List.filter (fun i -> not (List.mem i acc)) (succ graph idx)) @ tail)
        in search [] [start];;

    let bfs graph start =
        let rec search acc = function
            | []            -> List.rev acc
            | idx :: tail   -> search (idx :: acc) (tail @ (List.filter (fun i -> not (List.mem i acc)) (succ graph idx)))
        in search [] [start];;
end;;

module V = Vertex;;
module E = Edge(Vertex);;
module G = Graph(V)(E);;

let g = G.empty;;
let v1 = V.create 1;;
let v2 = V.create 2;;
let v3 = V.create 3;;
let v4 = V.create 4;;
let g = G.add_v g v1;;
let g = G.add_v g v2;;
let g = G.add_v g v3;;
let g = G.add_v g v4;;
let e1 = E.create 10 v1 v2;;
let e2 = E.create 11 v1 v3;;
let e3 = E.create 12 v3 v4;;
let g = G.add_e g e1;;
let g = G.add_e g e2;;
let g = G.add_e g e3;;

E.equal e1 e2;;
V.equal v1 v2;;
E.label e1;;
E.equal e1 e1;;
V.equal v1 v1;;
G.succ_e g v2;;

List.map V.label (G.dfs g v1);;
List.map V.label (G.bfs g v1);;
