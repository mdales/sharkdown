open Command
open Ordered_command

module DataFileSet = Set.Make(DataFile)

let process_to_dot command =
  (* let node_style = process_style node.style in *) (* TODO - some commands like littlejohn get different box styles*)
  let process_index = OrderedCommand.id command in
  List.iter ( fun datafile ->
      Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" (DataFile.id datafile) process_index
  ) (OrderedCommand.inputs command);
  Printf.fprintf Stdlib.stdout "\tn%d[shape=\"%s\",label=\"%s\"];\n" process_index "box" (Uri.pct_encode (Command.name (OrderedCommand.command command)));
  List.iter ( fun datafile ->
    Printf.fprintf Stdlib.stdout "\tn%d->n%d[penwidth=\"2.0\"];\n" process_index (DataFile.id datafile)
) (OrderedCommand.outputs command);
  Printf.fprintf Stdlib.stdout "\n"


let datafile_to_dot datafile =
  Printf.fprintf Stdlib.stdout "\tn%d[shape=\"cylinder\",label=\"%s\"];\n" (DataFile.id datafile) (DataFile.path datafile)


let project_to_dot (project : OrderedCommand.t list) : unit =
  Printf.fprintf Stdlib.stdout "digraph{\n";

  List.concat_map (fun command ->
    let inputs = OrderedCommand.inputs command
    and outputs = OrderedCommand.outputs command in
    List.concat [ inputs ; outputs]
  ) project |> DataFileSet.of_list |>
  DataFileSet.iter datafile_to_dot;

  List.iter process_to_dot project ;

  Printf.fprintf Stdlib.stdout "}\n"
