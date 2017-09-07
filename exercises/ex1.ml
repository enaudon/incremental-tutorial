(* Let's start with something very simple. Take a look at the
   functions in [Simple]. Your goal is to write incremental verisons
   of these same functions.

   Note that we don't expect a practical performance improvement here.
   the goal is just to get a sense of how to use the Incremental
   primitives.
*)

open! Core
open! Import
open! Incr.Let_syntax

type what_to_show = Volume | Footprint

module Simple = struct

  let metric what ~w ~h ~d =
    match what with
    | Volume -> w * h * d
    | Footprint -> w * d
  ;;

  let run () =
    let height = ref 50 in
    let width  = ref 120 in
    let depth  = ref 250 in
    let what = ref Footprint in
    (* This is an all-at-once computation *)
    let compute () =
      printf "%d\n" (metric !what ~w:!width ~h:!height ~d:!depth)
    in
    compute ();
    height := 150;
    width := 90;
    compute ();
    what := Volume;
    compute ();
  ;;

end

module Incremental = struct

  (* This should return the result as an incremental.

     Note, it's worth thinking about what the incremental graph looks
     like. E.g. if [watch=Footprint] then a change to [h] should not
     cause this node to refire. *)
  let metric (what:what_to_show Incr.t) ~(w:int Incr.t) ~(h: int Incr.t) ~(d:int Incr.t)
    : int Incr.t 
    =
    Incr.bind what ~f:(fun what -> match what with
      | Volume -> Incr.map3 w h d ~f:(fun w h d -> w * h * d)
      | Footprint -> Incr.map2 w d ~f:(fun w d -> w * d)
    )
  ;;    

  (* The structure of [run] should follow that of [simple_run] above
     closely, except:

     - OCaml references should be replaced with [Incr.Var.t]'s
     - [metric] should be called just once
     - An observer should be created based on the result of [metric]
     - [Incr.stabilize] needs to be called as part of [compute]
     - [compute] should then get its value using [Incr.Observer.value_exn].
  *)
  let run () : unit =
    let ref = Incr.Var.create in
    let (!) = Incr.Var.watch in
    let (:=) = Incr.Var.set in
    let height = ref 50 in
    let width  = ref 120 in
    let depth  = ref 250 in
    let what = ref Footprint in
    let obs =
        Incr.observe (metric !what ~w:!width ~h:!height ~d:!depth)
    in
    let compute () =
      Incr.stabilize ();
      printf "%d\n" (Incr.Observer.value_exn obs)
    in
    compute ();
    height := 150;
    width := 90;
    compute ();
    what := Volume;
    compute ();
  ;;

end

(* From here on is the declaration of the command-line interface,
   which you can mostly ignore for the purposes of the tutorial. *)
let command =
  let cmd ~summary run = Command.basic' ~summary (Command.Param.return run) in
  Command.group ~summary:"Exercise 1"
    [ "simple"      , cmd ~summary:"all-at-once implementation" Simple.run
    ; "incremental" , cmd ~summary:"incremental implementation" Incremental.run
    ]
