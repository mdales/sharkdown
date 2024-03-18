open Astring


module Command = struct
  type t = {
    name : string;
    args : string list;
    file_args : string list;
  }

  let find_file_args args =
    (* gross liberties, we assume for now that any arg with a doubeldash might be a file. though ultimately this
       will have to rely on convention, annotation, or guesses, so it's not exactly that bad, just limited as is. I imagine
       we can have a common prefix for all files, like example.com should be used for domains. *)
    List.filter (fun arg ->
      let regex = Str.regexp "^/data" in
      Str.string_match regex arg 0
    ) args

  let parse_python_command args =
    let name = match args with
    | "-m" :: x :: _ -> x
    | x :: _ -> x
    | _ -> "Unknown"
    in
    { name = name ; args = args ; file_args = find_file_args args }

  let parse_generic_commmand name args =
    { name = name ; args = args; file_args = find_file_args args }

  let of_string command_str =
    (* one day this will be all generic, but for now I'm hardcoding python just to make progress *)
    match (String.cuts ~sep:" " command_str) with
    | [] -> None
    | "python3" :: args -> Some (parse_python_command args)
    | name :: args -> Some (parse_generic_commmand name args)

  let name c = c.name

  let file_args c = c.file_args
end


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

  let command_list b =
    let regex_newline = Str.regexp "\\\\\n"
    and regex_comment = Str.regexp "#.*$"
    and regex_whitespace = Str.regexp "[\t ]+" in
    Str.global_replace regex_newline "" b.body |>
    Str.global_replace regex_comment "" |>
    String.cuts ~sep:"\n" |>
    List.map String.trim |>
    List.map (Str.global_replace regex_whitespace " ") |>
    List.filter_map (fun l ->
      match l with
      | "" -> None
      | x -> Some x
    )
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
        ((List.map Cmarkit.Block_line.to_string body) |> List.map String.trim |> String.concat ~sep:"\n")
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
    (* Initially ignore block scoping *)
    List.map Block.command_list ast |>
    List.concat |>
    List.map Command.of_string |>
    List.iter (fun c ->
      match c with
      | None -> Printf.printf "lost a command"
      | Some c -> (
        Printf.printf "%s\n" (Command.name c);
        List.iter (
          fun a ->
            Printf.printf "\t%s\n" a
        ) (Command.file_args c);
      )
    )


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
