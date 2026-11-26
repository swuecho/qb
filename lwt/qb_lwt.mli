(** Qb queries on Caqti_lwt connections, such as the ones [Dream.sql] hands out:

    {[
    Dream.sql req (fun db ->
        Qb.(from Users.table |> where Ops.(Users.id = 1) |> Qb_lwt.first db))
    ]} *)

include Qb.S with type 'a fiber = 'a Lwt.t and type db = Caqti_lwt.connection
