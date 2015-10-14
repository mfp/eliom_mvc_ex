open Eliom_lib

module React =
struct
  type 'a signal = 'a Lwt_react.signal
  type 'a event  = 'a Lwt_react.event

  module E = Lwt_react.E

  module S =
  struct
    include Lwt_react.S

    let keep_value s =
      let push v = function
        | Some _ as x -> x
        | None -> Some v in

      let define d =
        let d' = l2 push s d in
          (d', d')
      in
        map ~eq:(==)
          (function
             | Some x -> x
             | None ->
                 Lwt_log.ign_error_f "React_map.keep_value: None unexpected";
                 assert false) @@
        fix ~eq:(==) None define

    (* Issue: internal signals/events that depend on the supplied signal
     * are not stopped, causing a leak in absence of weak refs / GC finalizers
     * (i.e., under js_of_ocaml) *)
    let to_rlist s =
      let init, push = E.create () in
      let needs_init = ref true in
        ignore begin
          Lwt.pause () >>
          Lwt.catch
            (fun () ->
               if not !needs_init then
                 Lwt.return ()
               else begin
                 needs_init := false;
                 Lwt.return @@ push @@ value s
               end)
            (fun _ -> Lwt.return ())
        end;
        ReactiveData.RList.make_from (try value s with _ -> []) @@
        E.map (fun l -> needs_init := false; ReactiveData.RList.Set l) @@
        E.select [ changes s; init ]

    let space_safe_switch ?eq ss =
      let s = switch ?eq ss in
      let e = diff (fun _ old -> stop ~strong:true old) ss in
        l2 ~eq:(==) (fun s () -> s) s (hold () e)
  end
end

module ReactMap(O : Map.OrderedType) :
sig
  include module type of Map.Make(O)
  module React :
  sig
    (* val merge : 'a React.signal t -> 'a t React.signal *)
    val map_s :
      ?eq:('a -> 'a -> bool) ->
      (O.t -> 'a React.signal -> 'b React.signal) ->
      'a t React.signal -> 'b t React.signal

    val map :
      ?eq:('a -> 'a -> bool) ->
      (O.t -> 'a React.signal -> 'b) ->
      'a t React.signal -> 'b t React.signal
  end
end =
struct
  include Map.Make(O)

  module SSET = Set.Make(O)

  module React =
  struct
    open React

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

        S.space_safe_switch ~eq:(==) @@
        S.map ~eq:(==) merge @@
        S.fold ~eq:(==)
          (fun m (added, deleted) ->
             List.iter (fun k -> S.stop ~strong:true @@ find k m) deleted;
             let m = List.fold_left (fun m k -> remove k m) m deleted in
             let m = List.fold_left (fun m k -> add k (mk k) m) m added in
               m)
          init
          key_changes

    let map ?eq f m =
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
        let m =
          S.space_safe_switch ?eq @@
          S.map ~eq:(==)
            (fun init ->
               S.fmap ?eq
                 (fun m -> try Some (find k m) with Not_found -> None)
                 (find k init)
                 m) @@
          S.keep_value m
        in
          (f k m, m) in

      let signal init =
        S.fold ~eq:(==)
          (fun (m, m_) (added, deleted) ->
             List.iter
               (fun k ->
                  try S.stop ~strong:true @@ snd @@ find k m; with Not_found -> ())
               deleted;
             let m     = List.fold_left (fun m k -> remove k m) m deleted in
             let m_    = List.fold_left (fun m k -> remove k m) m_ deleted in
             let added = List.map (fun k -> (k, mk k)) added in
             let m     = List.fold_left (fun m (k, v) -> add k v m) m added in
             let m_    = List.fold_left (fun m (k, (v, _)) -> add k v m) m_ added in
               (m, m_))
          (init, map fst init)
          key_changes
      in
        S.map ~eq:(==) snd @@
        S.space_safe_switch ~eq:(==) @@
        S.map ~eq:(==) signal @@
        S.map ~eq:(==) (fun m -> fold (fun k v m -> add k (mk k) m) m empty) @@
        S.keep_value m
  end
end
