open Astring


module Block = struct
  type t = {
    kind : [ `Build | `Run  ];
    hash : string option;
    alias : string;
    body : string;
  }

  let of_info_string body info =
    match String.cut ~sep:":" info with
    | Some ("shark-build", env) ->
        Some { kind = `Build; hash = None; alias = env; body }
    | Some ("shark-run", env) ->
        Some { kind = `Run; hash = None; alias = env; body }
    | None ->
      Some { kind = `Run; hash = None; alias = info;  body }
    | _ -> None

  let body b = b.body
end

let parse_frontmatter frontmatter =
  match (String.trim frontmatter |> Yaml.of_string) with
  | Result.Ok _frontmatter -> Printf.printf ("Parsed frontmatter ok\n")
  | Result.Error _ -> Printf.eprintf "Failed to parse frontmatter\n"

let parse_markdown (markdown : string) : Block.t list =
  let doc = Cmarkit.Doc.of_string markdown in

  let blocks = ref [] in

  let block _ = function
  | Cmarkit.Block.Code_block (node, _meta) -> (
      let info_str = match Cmarkit.Block.Code_block.info_string node with
      | None -> "shark-run:"
      | Some (info_str, _) -> info_str
      in
      let body = Cmarkit.Block.Code_block.code node in
      let body =
        ((List.map Cmarkit.Block_line.to_string body) |> String.concat ~sep:"\n")
      in (
        match Block.of_info_string body info_str with
        | Some b -> blocks := b :: !blocks
        | None -> ()
      );
      `Default
  )
  | _ -> `Default
  in

  let mapper = Cmarkit.Mapper.make ~block () in
  ignore(Cmarkit.Mapper.map_doc mapper doc);
  List.rev !blocks

let parse_sharkdown file_path =
    let template = Eio.Path.load file_path in
    let ast = match String.cuts ~sep:"---" template with
    | [frontmatter; markdown] | [ ""; frontmatter; markdown ] ->
      parse_frontmatter frontmatter;
      parse_markdown markdown
    | [markdown] -> parse_markdown markdown
    | _ -> failwith "Malformed frontmatter/markdown file"
    in
    List.iter (fun b ->
      Printf.printf "%s\n" (Block.body b)
    ) ast

let () =
  Eio_main.run @@ fun env -> (
    let args = ref [] in
    Arg.parse [] (fun arg -> args := !args @ [arg])  "snarkdown <project markdown>";

    try
      match !args with
      | [] -> Printf.eprintf "Missing project markdown\n"; exit 1
      | x :: [] -> parse_sharkdown (Eio.Path.(Eio.Stdenv.fs env / x))
      | _ -> Printf.eprintf "Too many projects passed\n"; exit 1
    with
    | Unix.Unix_error (err, filename, _) -> Printf.eprintf "Failed to open %s: %s" filename (Unix.error_message err); exit 1
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Printf.eprintf("Todo : parse this error\n"); exit 1
  )
