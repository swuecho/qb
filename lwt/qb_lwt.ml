module Backend = struct
  type 'a fiber = 'a Lwt.t
  type ('a, 'e) stream = ('a, 'e) Caqti_lwt.Stream.t

  module type CONNECTION = Caqti_lwt.CONNECTION

  let return = Lwt.return
  let map = Lwt.map

  let finally f after =
    Lwt.finalize f (fun () ->
        after ();
        Lwt.return_unit)
end

include Qb.Make_guarded (Backend)
