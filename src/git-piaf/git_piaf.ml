open Lwt.Infix
module HTTP = Piaf.Client.Oneshot

let config = { Piaf.Config.default with max_redirects = 10 }

type error = Piaf.Error.t

let pp_error : error Fmt.t =
 fun ppf e -> Format.fprintf ppf "%a" Piaf.Error.pp_hum e

let get ~ctx:_ ?(headers = []) uri =
  HTTP.get ~config ~headers uri >>= function
  | Ok { Piaf.Response.body; _ } ->
    Piaf.Body.to_string body >|= fun body_result ->
    Result.map (fun body -> (), body) body_result
  | Error e ->
    Lwt.return_error e

let post ~ctx:_ ?(headers = []) uri body =
  let body = Piaf.Body.of_string body in
  HTTP.post ~config ~headers ~body uri >>= function
  | Ok { Piaf.Response.body; _ } ->
    Piaf.Body.to_string body >|= fun body_result ->
    Result.map (fun body -> (), body) body_result
  | Error e ->
    Lwt.return_error e
