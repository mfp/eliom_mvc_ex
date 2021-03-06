open Eliom_lib

module Make(O : Map.OrderedType) :
sig
  include module type of Map.Make(O)
  module React :
  sig
    (* val merge : 'a React.signal t -> 'a t React.signal *)
    val map_s :
      ?eq:('a -> 'a -> bool) ->
      (O.t -> 'a React.signal -> 'b React.signal) ->
      'a t React.signal -> 'b t React.signal
  end
end =
struct
  include Map.Make(O)

  module SSET = Set.Make(O)

  module React =
  struct
    open React

    let safe_switch ?eq s =
      let e = S.diff (fun _ old -> S.stop ~strong:true old) s in
      let s = S.switch ?eq s in
        S.l2 (fun s () -> s) s (S.hold () e)

    (* cannot be used in update cycle: S.value fails
    let merge m =
      let changes =
        fold
          (fun k v l ->
             let e = S.changes v |> E.map (fun x -> (k, x)) in
               e :: l)
          m [] |>
        E.merge (fun l e -> e :: l) []
      in
        S.fold ~eq:(==)
          (fun m l -> List.fold_left (fun m (k, v) -> add k v m) m l)
          (map S.value m)
          changes
     *)

    let keys m = fold (fun k _ l -> k :: l) m []

    let merge m =
      S.map ~eq:(==)
        (fun l -> List.fold_left (fun m (k, v) -> add k v m) empty l) @@
      S.merge ~eq:(==) (fun l x -> x :: l) [] @@
      fold (fun k v l -> S.Pair.pair (S.const k) v :: l) m []

    (* FIXME: implement without using S.value (same issue as above) *)
    let map_s ?(eq = (=)) f m =
      let key_changes =
        S.diff
          (fun m2 m1 ->
             let b1      = SSET.of_list @@ keys m1 in
             let b2      = SSET.of_list @@ keys m2 in
             let deleted = SSET.elements @@ SSET.diff b1 b2 in
             let added   = SSET.elements @@ SSET.diff b2 b1 in
               (added, deleted)) @@
          m in

      let mk k =
        f k @@
        S.fmap ~eq
          (fun m -> try Some (find k m) with Not_found -> None)
          (find k @@ S.value m)
          m in

      let init = fold (fun k v m -> add k (mk k) m) (S.value m) empty in

        safe_switch ~eq:(==) @@
        S.map ~eq:(==) merge @@
        S.fold ~eq:(==)
          (fun m (added, deleted) ->
             List.iter (fun k -> S.stop ~strong:true @@ find k m) deleted;
             let m = List.fold_left (fun m k -> remove k m) m deleted in
             let m = List.fold_left (fun m k -> add k (mk k) m) m added in
               m)
          init
          key_changes
  end
end
