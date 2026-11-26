(* Finds libpg_query, which ships no pkg-config file. Tried in order:
   LIBPG_QUERY_PREFIX, pkg-config (some distros add one), Homebrew, then the
   compiler's default search path. *)

module C = Configurator.V1

let flags_for prefix =
  ( [ "-I" ^ Filename.concat prefix "include" ],
    [ "-L" ^ Filename.concat prefix "lib"; "-lpg_query" ] )

let has_header prefix =
  Sys.file_exists (Filename.concat prefix "include/pg_query.h")

let brew_prefix c =
  match C.which c "brew" with
  | None -> None
  | Some brew -> (
      match C.Process.run c brew [ "--prefix"; "libpg_query" ] with
      | { exit_code = 0; stdout; _ } ->
          let p = String.trim stdout in
          if has_header p then Some p else None
      | _ -> None)

let () =
  C.main ~name:"libpg_query" (fun c ->
      let cflags, libs =
        match Sys.getenv_opt "LIBPG_QUERY_PREFIX" with
        | Some p when p <> "" -> flags_for p
        | _ -> (
            let pc =
              match C.Pkg_config.get c with
              | None -> None
              | Some pc -> C.Pkg_config.query pc ~package:"libpg_query"
            in
            match pc with
            | Some { cflags; libs } -> (cflags, libs)
            | None -> (
                match brew_prefix c with
                | Some p -> flags_for p
                | None ->
                    List.find_opt has_header [ "/usr/local"; "/opt/local" ]
                    |> Option.fold ~none:([], [ "-lpg_query" ]) ~some:flags_for)
            )
      in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" libs)
