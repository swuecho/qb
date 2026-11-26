/* The one libpg_query call qb-pg-parse needs: parse SQL to a JSON tree. */

#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <pg_query.h>

/* Ok json | Error (message, cursorpos), as an OCaml result. cursorpos is
   1-based in libpg_query, 0 when unknown. */
CAMLprim value qb_pg_query_parse(value v_sql)
{
  CAMLparam1(v_sql);
  CAMLlocal3(v_res, v_payload, v_str);

  /* libpg_query reads up to the first NUL, so a string holding one would be
     parsed only in part. */
  if (strlen(String_val(v_sql)) != caml_string_length(v_sql)) {
    v_str = caml_copy_string("SQL text contains a NUL byte");
    v_payload = caml_alloc_tuple(2);
    Store_field(v_payload, 0, v_str);
    Store_field(v_payload, 1, Val_int(0));
    v_res = caml_alloc(1, 1);
    Store_field(v_res, 0, v_payload);
    CAMLreturn(v_res);
  }

  PgQueryParseResult r = pg_query_parse(String_val(v_sql));

  if (r.error != NULL) {
    v_str = caml_copy_string(r.error->message ? r.error->message : "");
    v_payload = caml_alloc_tuple(2);
    Store_field(v_payload, 0, v_str);
    Store_field(v_payload, 1, Val_int(r.error->cursorpos));
    v_res = caml_alloc(1, 1); /* Error */
  } else {
    v_payload = caml_copy_string(r.parse_tree ? r.parse_tree : "{}");
    v_res = caml_alloc(1, 0); /* Ok */
  }
  Store_field(v_res, 0, v_payload);
  pg_query_free_parse_result(r);
  CAMLreturn(v_res);
}
