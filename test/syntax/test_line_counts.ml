let max_lines = 1550

let rec find_project_root dir remaining =
  let has name = Sys.file_exists (Filename.concat dir name) in
  if has "dune-project" && has "lib" then dir
  else if remaining = 0 then Alcotest.failf "could not find project root from %s" (Sys.getcwd ())
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then Alcotest.failf "could not find project root from %s" (Sys.getcwd ())
    else find_project_root parent (remaining - 1)

let is_impl_source path =
  match Filename.extension path with
  | ".ml" | ".mli" -> true
  | _ -> false

let rec collect_sources dir =
  if not (Sys.file_exists dir) then []
  else if Sys.is_directory dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.concat_map (fun name -> collect_sources (Filename.concat dir name))
  else if is_impl_source dir then [ dir ]
  else []

let count_lines path =
  let input = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
      let rec loop count =
        match input_line input with
        | _ -> loop (count + 1)
        | exception End_of_file -> count
      in
      loop 0)

let relative_path root path =
  let prefix = root ^ Filename.dir_sep in
  let prefix_len = String.length prefix in
  if String.length path > prefix_len && String.equal (String.sub path 0 prefix_len) prefix then
    String.sub path prefix_len (String.length path - prefix_len)
  else path

let test_implementation_sources_stay_under_limit () =
  let root = find_project_root (Sys.getcwd ()) 12 in
  let sources = [ "lib"; "bin" ] |> List.concat_map (fun dir -> collect_sources (Filename.concat root dir)) in
  let offenders =
    sources
    |> List.filter_map (fun path ->
           let lines = count_lines path in
           if lines > max_lines then Some (relative_path root path, lines) else None)
  in
  match offenders with
  | [] -> ()
  | _ ->
      let details =
        offenders
        |> List.sort (fun (_, a) (_, b) -> compare b a)
        |> List.map (fun (path, lines) -> Printf.sprintf "%s: %d lines" path lines)
        |> String.concat "\n"
      in
      Alcotest.failf "implementation sources must stay under %d lines:\n%s" max_lines details

let suites =
  [ ( "line counts",
      [ Alcotest.test_case "implementation sources stay under 1500 lines" `Quick
          test_implementation_sources_stay_under_limit ] ) ]
