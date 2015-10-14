{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Lwt

  module React = Eliom_mvc_lib.React

  open React
}}

{client{

module CD = Eliom_mvc_lib.Cheap_diff

(* Issue: internal signals/events that depend on the supplied signal
 * are not stopped, causing a leak in absence of weak refs / GC finalizers
 * (i.e., under js_of_ocaml) *)
let to_rlist_with_delta s =
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
             Lwt.return @@ push @@ ([], S.value s)
           end)
        (fun _ -> Lwt.return ())
    end;
    ReactiveData.RList.make_from (try S.value s with _ -> []) @@
    E.map
      (fun (l0, l1) ->
         needs_init := false;

         let patches =
           CD.diff
             (fun elm ->
                match Eliom_content.Xml.get_node_id @@
                      Eliom_content.Html5.D.toelt elm
                with
                  | Eliom_content.Xml.NoId -> None
                  | x -> Some x)
             compare
             l0 l1 in

         let () =
           List.iter
             (function
                | CD.Add (n, _) -> Lwt_log.ign_debug_f "XXX Add (%d, _)" n
                | CD.Del n -> Lwt_log.ign_debug_f "XXX Del (%d, _)" n)
             patches in

         let patches =
           List.map
             (function
                | CD.Add (n, x) -> ReactiveData.RList.I (n, x)
                | CD.Del n -> ReactiveData.RList.R n)
             patches
         in
           Lwt_log.ign_debug_f "XXX %d patches" (List.length patches);
           ReactiveData.RList.Patch patches) @@
    E.select [ S.diff (fun l1 l0 -> (l0, l1)) s; init ]

module R = Html5.R

module Counter =
struct
  type model = int
  type action = [`Inc | `Dec]

  open Eliom_mvc.Effect
  let update push a m = match a with
    | `Inc -> no_effect @@ m + 1
    | `Dec -> no_effect @@ m - 1

  type context = unit
  type view = [`Span]

  let view () push m =
    let dec = button ~button_type:`Button
                ~a:[a_onclick (fun _ -> push `Dec)] [pcdata "-"] in
    let inc = button ~button_type:`Button
                ~a:[a_onclick (fun _ -> push `Inc)] [pcdata "+"] in
      span
        [ dec; pcdata " ";
          span [ R.pcdata @@ S.map string_of_int m ]; pcdata " "; inc ]
end

module Thing =
struct
  type model =
      { id : string; desc : string; count : int; status : [`Edit | `Normal] }

  let make ~id ?(desc = "") ?(count = 1) ?(status = `Normal) () =
    { id; desc; count; status }

  type action =
      | Edit_desc of [`Start | `Finish of string]
      | Counter of Counter.action
      | Set_focus of [`Input] elt

  open Eliom_mvc.Effect

  let update push a m = match a with
    | Edit_desc `Start -> no_effect { m with status = `Edit }
    | Edit_desc (`Finish desc ) -> no_effect { m with desc; status = `Normal; }
    | Counter a ->
        let push_c a = push @@ Counter a in
        let count, effect = Counter.update push_c a m.count in
          { m with count } &&! effect
    | Set_focus elm ->
        m &&! (fun () -> (Html5.To_dom.of_input elm)##focus())

  type context = unit Eliom_mvc.push_action
  type view = [`Tr] elt

  let class_of_status = function
    | `Edit -> "edit"
    | `Normal -> ""

  let view delete_self push m =
    let del_btn = button
                    ~button_type:`Button
                    ~a:[a_onclick (fun _ -> delete_self ())]
                    [ pcdata "X" ] in
    let counter = S.map (fun m -> m.count) m |>
                  Counter.view () (fun x -> push @@ Counter x) in

    let desc =
      R.node @@
      S.map
        (fun m -> match m.status with
           | `Normal ->
               td ~a:[a_class ["desc"; "click-to-edit"];
                      a_onclick (fun _ -> push @@ Edit_desc `Start)]
               [ pcdata m.desc ]
           | `Edit ->
             let desc  = Raw.input ~a:[a_value m.desc] () in
               Eliom_mvc.Handler.on_enter_or_blur desc
                 (fun value -> push @@ Edit_desc (`Finish value));
               push (Set_focus desc);
               td ~a:[a_class ["desc"]] [desc]) @@
      m
    in
      Lwt_log.ign_debug_f "Render Thing";
      tr ~a:[R.a_class @@
             S.map (fun m -> [ "thing"; class_of_status m.status ]) m]
        [
          td ~a:[a_class ["id"]]
            [ del_btn; pcdata " "; R.pcdata @@ S.map (fun m -> m.id) m ];
          td ~a:[a_class ["counter"]] [counter];
          desc;
        ]
end

module ThingList =
struct
  module M = Eliom_mvc_lib.ReactMap(String)

  type model =
      {
        id     : string;
        status : [`OK | `Error];
        things : Thing.model M.t;
      }

  let make things =
    {
      id = "";
      status   = `OK;
      things   = List.fold_left (fun m t -> M.add t.Thing.id t m) M.empty things;
    }

  open Printf

  type id = string

  type action =
    | Add_thing
    | Set_id of id
    | Delete_thing of id
    | Thing_action of id * Thing.action

  let string_of_action = function
    | Add_thing -> "Add_thing"
    | Set_id id -> sprintf "Set_id %s" id
    | Delete_thing id -> sprintf "Delete_thing %s" id
    | Thing_action (id, _) -> sprintf "Thing_action %s" id

  open Eliom_mvc.Effect

  let update push a m = match a with
    | Add_thing -> begin
        match m.id, M.mem m.id m.things with
          | "", _ | _, true ->
              no_effect @@ { m with id = ""; status = `Error }
          | _ ->
              let thing = Thing.make ~id:m.id () in
                no_effect @@
                { status = `OK;
                  id = "";
                  things = M.add m.id thing m.things;
                }
      end
    | Set_id id -> no_effect @@ { m with id; status = `OK }
    | Delete_thing id ->
        no_effect @@ { m with things = M.remove id m.things }
    | Thing_action (id, a) ->
        match M.find id m.things with
          | exception Not_found -> no_effect m
          | thing ->
              let thing, effect =
                Thing.update (fun a -> push @@ Thing_action (id, a)) a thing
              in
                { m with things = M.add id thing m.things } &&! effect

  type context = unit
  type view    = [`Div] elt

  let class_of_status = function
    | `Error -> "error"
    | `OK -> ""

  let view () push m =
    let id_input  = Raw.input ~a:[R.a_value @@ S.map (fun m -> m.id) m] () in
    let id_input_ = Html5.To_dom.of_input id_input in

    let add_btn   =
      button
        ~button_type:`Submit
        ~a:[a_onclick (fun _ ->
                         push @@ Set_id (Js.to_string id_input_##value);
                         push Add_thing)]
        [ pcdata "Add" ] in

    let things = S.map ~eq:(==) (fun m -> m.things) m in

    let push_delete id () = push @@ Delete_thing id in

    let thing_views =
      M.React.map
        (fun id t ->
           Thing.view
             (push_delete id)
             (fun a -> push @@ Thing_action (id, a)) t)
        things in

    let rows =
      to_rlist_with_delta @@
      S.map ~eq:(==) (fun m -> List.rev @@ M.fold (fun _ v l -> v :: l) m [])
        thing_views
    in
      Lwt_log.ign_debug_f "Render list of Things";

      div ~a:[a_class ["thing-list"]]
        [
          Raw.form
            ~a:[
              R.a_class @@
              S.map (fun m -> ["controls"; class_of_status m.status]) m;
              a_onsubmit (fun ev -> Dom.preventDefault ev);
            ]
            [ id_input; pcdata " "; add_btn ];
          tablex
            ~a:[a_class ["things"]]
            ~thead:(thead
                      [ tr [ th ~a:[a_class ["id"]] [ pcdata "id"];
                             th ~a:[a_class ["count"]] [ pcdata "count"];
                             th ~a:[a_class ["desc"]] [ pcdata "desc"]; ] ])
            [R.tbody rows]
        ]
end
}}

module Eliom_mvc_ex_app =
  Eliom_registration.App (
    struct
      let application_name = "eliom_mvc_ex"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Eliom_mvc_ex_app.register
    ~service:main_service
    (fun () () ->
       let content = div [] in
         ignore {unit{
           Eliom_mvc.run
             ~trace:(fun a -> Lwt_log.ign_debug_f "Action %s" @@
                              ThingList.string_of_action a)
             %content
             ThingList.update
             (ThingList.view ())
             (ThingList.make [])
         }};
         Lwt.return
           (Eliom_tools.F.html
              ~title:"eliom_mvc_ex"
              ~css:[["css";"eliom_mvc_ex.css"]]
              Html5.F.(body [
                h2 [pcdata "Welcome from Eliom MVC"; ];
                content;
              ])))

(* vim: set ft=ocaml: *)
