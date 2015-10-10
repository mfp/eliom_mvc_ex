{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
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
      Lwt.return
        (Eliom_tools.F.html
           ~title:"eliom_mvc_ex"
           ~css:[["css";"eliom_mvc_ex.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
