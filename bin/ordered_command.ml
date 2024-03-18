open Command

module OrderedCommand = struct
  type t = {
    command : Command.t;
    inputs : string list;
    outputs : string list;
  }

  let command o = o.command

  let inputs o = o.inputs

  let outputs o = o.outputs

  let order_command_list (commands : Command.t list) : t list =

    let rec loop commands known_files =

      match commands with
      | [] -> []
      | hd :: tl -> (
        let file_args = Command.file_args hd in

        let is_inputs = List.map (fun x ->
          match List.find_opt (fun t -> t = x) known_files with
          | None -> (x, false)
          | Some _ -> (x, true)
        ) file_args in

        let inputs = List.filter_map (fun x ->
          match x with
          | _, false -> None
          | f, true -> Some f
        ) is_inputs in
        let outputs = List.filter_map (fun x ->
          match x with
          | f, false -> Some f
          | _, true -> None
        ) is_inputs in

        let x = {
          command = hd;
          inputs = inputs;
          outputs = outputs;
        } in
        x :: (loop tl (List.concat [ known_files ; outputs]))
      )
    in
    loop commands []


end