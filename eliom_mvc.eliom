
{shared{
open Eliom_lib

type 'a push_action = 'a -> unit

type effect = unit -> unit

module type COMPONENT =
sig
  type model
  type action
  type context
  type view

  val update : action push_action -> action -> model -> model * effect

  val view :
    context -> action push_action ->
    model React.signal -> view Eliom_content.Html5.elt
end

module Effect =
struct
  let nothing () = ()

  let (&&!) a b = (a, b)

  let no_effect x = x &&! nothing

  let const v _ = v
end

}}

{client{

module Handler =
struct
  let on_enter_or_blur input f =
    let elm = Eliom_content.Html5.To_dom.of_input input in
      Lwt.async
        (fun () ->
           Lwt_js_events.keypresses elm
             (fun ev _ ->
                if ev##keyCode <> 13 then Lwt.return_unit
                else Lwt.return @@ f @@ Js.to_string elm##value));
      Lwt.async
        (fun () ->
           Lwt_js_events.blurs elm
             (fun _ _ -> Lwt.return @@ f @@ Js.to_string elm##value))
end


let run ?trace parent update view init =
  let open Lwt_react in
  let events, push = Lwt_stream.create () in

  let push x = push @@ Some x in
  let events = E.of_stream events in

  let events = match trace with
    | None -> events
    | Some f -> E.trace f events in

  let model_and_action =
    S.fold ~eq:(==)
      (fun (m, _) a ->
         try update push a m
         with exn ->
           Lwt_log.ign_error_f "Exception in model update: %s"
             (Printexc.to_string exn);
           raise exn)
      (init, (fun () -> ()))
      events in

  let model  = S.Pair.fst ~eq:(==) model_and_action in
  let action = S.Pair.snd ~eq:(==) model_and_action in

  let view   = try view push model
               with exn ->
                 Lwt_log.ign_error_f "Exception raised by view: %s" @@
                 Printexc.to_string exn;
                 raise exn
  in
    (* initial load *)
    Eliom_content.Html5.Manip.replaceChildren parent [view];
    (* actions *)
    E.keep @@
    S.diff (fun action _ -> action ()) @@
    action
}}

(* vim: set ft=ocaml: *)
