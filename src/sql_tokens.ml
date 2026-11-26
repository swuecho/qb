(* SQL text cut into the pieces that matter to code looking for placeholders:
   what is a string, a quoted name, a comment or a $$-quoted body, where
   nothing is a placeholder, and what is $1, ? or :name outside those.

   This is not a SQL parser. The statements themselves are read by
   libpg_query in qb-pg-parse; this only finds placeholders in text on its
   way there or to the database.

   One file, compiled into both qb and qb-pg-parse, so that the two agree on
   where a string ends without either depending on the other: qb-pg-parse
   copies it when it is built (see pg-parse/src/dune). Edit it here. *)

type kind =
  | Text  (** Anything else: words, spaces, operators, [::]. *)
  | Quoted
      (** ['a string'], [E'with \' escapes'], ["a name"], [$tag$ a body $tag$].
      *)
  | Comment  (** [-- to the end of the line] or [/* ... */]. *)
  | Numbered of int  (** [$1] *)
  | Question  (** [?] *)
  | Named of string  (** [:name]; the [name]. *)

type token = {
  at : int;  (** Byte offset in the text. *)
  text : string;  (** The token as written: the texts, joined, are the input. *)
  kind : kind;
}

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_digit c = c >= '0' && c <= '9'
let is_ident c = is_ident_start c || is_digit c

open Angstrom

(* What is left unclosed runs to the end of the text. *)
let until_char q =
  skip_while (fun c -> c <> q) *> (char q *> return () <|> end_of_input)

let until close =
  fix (fun more ->
      end_of_input <|> string close *> return () <|> any_char *> more)

let string_ = char '\'' *> until_char '\''
let name = char '"' *> until_char '"'

(* E'...': a backslash escapes the next character. Tried at the start of a
   word only, so the E of NAME'...' isn't one. *)
let e_string =
  (char 'E' <|> char 'e')
  *> char '\''
  *> fix (fun more ->
      end_of_input
      <|> ( any_char >>= function
            | '\\' -> any_char *> more <|> end_of_input
            | '\'' -> return ()
            | _ -> more ))

(* $tag$ ... $tag$. A tag doesn't start with a digit, so $1$ isn't one. *)
let dollar_quoted =
  char '$' *> take_while is_ident <* char '$' >>= fun tag ->
  if tag <> "" && is_digit tag.[0] then fail "not a tag"
  else until ("$" ^ tag ^ "$")

let line_comment = string "--" *> skip_while (fun c -> c <> '\n')
let block_comment = string "/*" *> until "*/"

let numbered =
  char '$' *> take_while1 is_digit >>| fun digits ->
  match int_of_string_opt digits with Some n -> Numbered n | None -> Text

let named =
  char ':' *> consumed (satisfy is_ident_start *> skip_while is_ident)
  >>| fun name -> Named name

(* A run with nothing that could start another kind of token. *)
let plain c =
  not
    (is_ident c
    ||
    match c with
    | '\'' | '"' | '-' | '/' | '$' | '?' | ':' -> true
    | _ -> false)

let kind =
  choice
    [
      e_string *> return Quoted;
      string_ *> return Quoted;
      name *> return Quoted;
      line_comment *> return Comment;
      block_comment *> return Comment;
      dollar_quoted *> return Quoted;
      numbered;
      (* A cast, x::text, is not a parameter called text. *)
      string "::" *> return Text;
      named;
      char '?' *> return Question;
      take_while1 is_ident *> return Text;
      take_while1 plain *> return Text;
      any_char *> return Text;
    ]

let token = lift3 (fun at kind stop -> (at, kind, stop)) pos kind pos

let tokens sql =
  match parse_string ~consume:All (many token) sql with
  | Ok toks ->
      List.map
        (fun (at, kind, stop) ->
          { at; text = String.sub sql at (stop - at); kind })
        toks
  (* Every character is some token, so this isn't reached. *)
  | Error _ -> [ { at = 0; text = sql; kind = Text } ]
