{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Lwt
}}

{client{

module Counter
  : Eliom_mvc.COMPONENT with type Model.t = int
                         and type Action.t = [`Inc | `Dec]
                         and type View.context = unit
                         and type View.view = [`Span] elt =
struct
  module Model = struct type t = int end
  module Action = struct type t = [`Inc | `Dec] end

  module Controller =
  struct
    open Eliom_mvc.Effect

    let update push a m = match a with
      | `Inc -> no_effect @@ m + 1
      | `Dec -> no_effect @@ m - 1
  end

  module View =
  struct
    open React

    type context = unit
    type view = [`Span] elt

    let signal () push m =
      let dec = button ~button_type:`Button
                  ~a:[a_onclick (fun _ -> push `Dec)] [pcdata "-"] in
      let inc = button ~button_type:`Button
                  ~a:[a_onclick (fun _ -> push `Inc)] [pcdata "+"] in

        S.map ~eq:(==)
          (fun n ->
             span
               [ dec; pcdata " ";
                 span [ pcdata @@ string_of_int n ]; pcdata " "; inc ])
          m
  end
end

module Thing =
struct
  module Model =
  struct
    type t = { id : string; desc : string; count : int; status : [`Edit | `Normal] }
  end

  let make ~id ?(desc = "") ?(count = 1) ?(status = `Normal) () =
    { Model.id; desc; count; status }

  module Action =
  struct
    type t =
      | Edit_desc of [`Start | `Finish of string]
      | Counter of Counter.Action.t
      | Set_focus of [`Input] elt
  end

  module Controller =
  struct
    open Model
    open Action
    open Eliom_mvc.Effect

    let update push a m = match a with
      | Edit_desc `Start -> no_effect { m with status = `Edit }
      | Edit_desc (`Finish desc ) -> no_effect { m with desc; status = `Normal; }
      | Counter a ->
          let push_c a = push @@ Counter a in
          let count, effect = Counter.Controller.update push_c a m.count in
            { m with count } &&! effect
      | Set_focus elm ->
          m &&! (fun () -> (Html5.To_dom.of_input elm)##focus())
  end

  module View =
  struct
    open Model
    open React
    open Action

    type context = unit Eliom_mvc.push_action
    type view = [`Tr] elt

    let class_of_status = function
      | `Edit -> "edit"
      | `Normal -> ""

    let signal delete_self push m =
      let del_btn = button
                      ~button_type:`Button
                      ~a:[a_onclick (fun _ -> delete_self ())]
                      [ pcdata "X" ] in
      let counter = S.map (fun m -> m.count) m |>
                    Counter.View.signal () (fun x -> push @@ Counter x) in

      let mk_desc m = match m.status with
        | `Normal ->
            td ~a:[a_class ["desc"; "click-to-edit"];
                   a_onclick (fun _ -> push @@ Edit_desc `Start)]
            [ pcdata m.desc ]
        | `Edit ->
          let desc  = Raw.input ~a:[a_value m.desc] () in
            Eliom_mvc.Handler.on_enter_or_blur desc
              (fun value -> push @@ Edit_desc (`Finish value));
            push (Set_focus desc);
            td ~a:[a_class ["desc"]] [desc]
      in
        S.l2 ~eq:(==)
          (fun m counter ->
             Lwt_log.ign_debug_f "Render Thing";
             tr ~a:[a_class ["thing"; class_of_status m.status]]
               [
                 td ~a:[a_class ["id"]]
                   [ del_btn; pcdata " "; pcdata m.id ];
                 td ~a:[a_class ["counter"]] [counter];
                 mk_desc m;
               ])
          m counter
  end
end

module ThingList =
struct
  module M = React_map.Make(String)

  module Model =
  struct
    type t =
        {
          id     : string;
          status : [`OK | `Error];
          things : Thing.Model.t M.t;
        }
  end

  let make things =
    {
      Model.id = "";
      status   = `OK;
      things   = List.fold_left
                   (fun m t -> M.add t.Thing.Model.id t m) M.empty things;
    }

  module Action =
  struct
    open Printf

    type id = string

    type t =
      | Add_thing
      | Set_id of id
      | Delete_thing of id
      | Thing_action of id * Thing.Action.t

    let string_of_action = function
      | Add_thing -> "Add_thing"
      | Set_id id -> sprintf "Set_id %s" id
      | Delete_thing id -> sprintf "Delete_thing %s" id
      | Thing_action (id, _) -> sprintf "Thing_action %s" id
  end

  module Controller =
  struct
    open Eliom_mvc.Effect
    open Model
    open Action

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
                  Thing.Controller.update
                    (fun a -> push @@ Thing_action (id, a))
                    a thing
                in
                  { m with things = M.add id thing m.things } &&! effect
  end

  module View =
  struct
    open React
    open Model
    open Action

    type context = unit
    type view    = [`Div] elt

    let class_of_status = function
      | `Error -> "error"
      | `OK -> ""

    let signal () push m =
      (* We can hoist element definition outside the view signal so as to
       * use elements with DOM semantics (thus allowing to preserve view state
       * like input contents, cursor position, etc., which we don't really
       * want to include in the model itself. *)
      let id_input  = Raw.input ~a:[a_value @@ (S.value m).id] () in
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
        M.React.map_s
          (fun id t ->
             Thing.View.signal
               (push_delete id)
               (fun a -> push @@ Thing_action (id, a))
               t)
          things
      in
        S.l2 ~eq:(==)
          (fun m thing_views ->
             let rows = List.rev @@ M.fold (fun _ v l -> v :: l) thing_views [] in

               id_input_##value <- Js.string m.id;

               Lwt_log.ign_debug_f "Render list of %d Things" @@ List.length rows;

               div ~a:[a_class ["thing-list"]]
                 [
                   Raw.form
                     ~a:[a_class ["controls"; class_of_status m.status];
                         a_onsubmit (fun ev -> Dom.preventDefault ev);
                        ]
                     [ id_input; pcdata " "; add_btn ];
                   tablex
                     ~a:[a_class ["things"]]
                     ~thead:(thead
                               [ tr [ th ~a:[a_class ["id"]] [ pcdata "id"];
                                      th ~a:[a_class ["count"]] [ pcdata "count"];
                                      th ~a:[a_class ["desc"]] [ pcdata "desc"]; ] ])
                     [tbody rows]
                 ])
          m thing_views
  end


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
                              ThingList.Action.string_of_action a)
             %content
             ThingList.Controller.update
             (fun push m -> React.S.map (fun x -> [x]) @@
                            ThingList.View.signal () push m)
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
