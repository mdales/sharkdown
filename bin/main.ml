open Astring
open Block
open Command
open Frontmatter
open Ordered_command

let parse_frontmatter frontmatter =
  match (Frontmatter.of_string frontmatter) with
  | Some frontmatter -> frontmatter
  | None -> failwith "Failed to parse frontmatter\n"

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
    let metadata, ast = match String.cuts ~sep:"---" template with
    | [frontmatter; markdown] | [ ""; frontmatter; markdown ] ->
      (parse_frontmatter frontmatter, parse_markdown markdown)
    | [markdown] -> (Frontmatter.empty, parse_markdown markdown)
    | _ -> failwith "Malformed frontmatter/markdown file"
    in
    (* Initially ignore block scoping *)
    List.map Block.command_list ast |>
    List.concat |>
    List.filter_map Command.of_string |>

    OrderedCommand.order_command_list metadata |>

    List.iter (fun oc ->
      let c = OrderedCommand.command oc in
      Printf.printf "%s\n" (Command.name c);
      Printf.printf "\tInputs:\n";
      List.iter (
        fun a ->
          Printf.printf "\t\t%s\n" a
      ) (OrderedCommand.inputs oc);
      Printf.printf "\tOutputs:\n";
      List.iter (
        fun a ->
          Printf.printf "\t\t%s\n" a
      ) (OrderedCommand.outputs oc);
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
