open Cmdliner
open Lwt.Infix

module Conf = struct
  let stable_hash = 32

  let entries = 256
end

let () =
  Logs.set_level (Some Logs.App);
  Logs.set_reporter (Logs_fmt.reporter ())

let http_callback (type x) (module Store : Irmin.S with type t = x) (store : x)
    _conn _req body =
  let repo = Store.repo store in
  Cohttp_lwt.Body.drain_body body >>= fun () ->
  let body = Irmin.Type.to_string (Store.Status.t repo) (Store.status store) in
  Cohttp_lwt_unix.Server.respond_string ~body ~status:`OK ()

let run root host port secret_key address_file insecure max_tx =
  let module Store =
    Irmin_pack_layered.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Irmin.Hash.BLAKE2B)
  in
  let module Rpc =
    Irmin_rpc_unix.Make
      (Store)
      (Irmin_rpc.Config.Remote.None (Store))
      (Irmin_rpc.Config.Pack.None (Store))
  in
  let secret_key =
    match secret_key with Some key -> `File key | None -> `Ephemeral
  in
  let secure = not insecure in
  let conf = Irmin_pack.config root in
  let config =
    Irmin_pack_layered.config ~conf ~copy_in_upper:true ~with_lower:true ()
  in
  let p =
    Store.Repo.v config >>= fun repo ->
    Store.master repo >>= fun store ->
    Rpc.Server.serve ?max_tx ~secure ~secret_key (`TCP (host, port)) repo
    >>= fun server ->
    let () =
      match address_file with
      | Some f ->
          let f = open_out f in
          output_string f (Uri.to_string (Rpc.Server.uri server));
          close_out f
      | None ->
          Logs.app (fun l -> l "%s" (Uri.to_string (Rpc.Server.uri server)))
    in

    let http =
      Cohttp_lwt_unix.Server.make
        ~callback:(http_callback (module Store) store)
        ()
    in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port (port + 1))) http
  in
  Lwt_main.run p

let host =
  let doc = "Server address" in
  Arg.(
    value & opt string "127.0.0.1" & info [ "a"; "address" ] ~docv:"HOST" ~doc)

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 9998 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let secret_key =
  let doc = "Secret key" in
  Arg.(
    value
    & opt (some string) None
    & info [ "k"; "secret-key" ] ~docv:"FILENAME" ~doc)

let address_file =
  let doc = "Write address to file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "f"; "address-file" ] ~docv:"FILENAME" ~doc)

let insecure =
  let doc = "Disable SSL and other security features" in
  Arg.(value & flag & info [ "insecure" ] ~doc)

let max_tx =
  let doc = "Maximum number of open transactions per client" in
  Arg.(value & opt (some int) None & info [ "x"; "max-tx" ] ~docv:"MAX" ~doc)

let root =
  let doc = "Root directory for Irmin store" in
  Arg.(
    value & opt string "/tmp/irmin-rpc" & info [ "r"; "root" ] ~docv:"PATH" ~doc)

let main_t =
  Term.(
    const run
    $ root
    $ host
    $ port
    $ secret_key
    $ address_file
    $ insecure
    $ max_tx)

let () = Term.exit @@ Term.eval (main_t, Term.info "irmin-rpc")
