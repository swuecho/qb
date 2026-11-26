(* Sql_tokens: what is a placeholder, and what only looks like one. *)

open Sql_tokens

let show t =
  match t.kind with
  | Text -> "text " ^ t.text
  | Quoted -> "quoted " ^ t.text
  | Comment -> "comment " ^ t.text
  | Numbered n -> Printf.sprintf "$%d" n
  | Question -> "?"
  | Named n -> ":" ^ n

(* The tokens that aren't plain text. *)
let marks sql =
  List.filter_map
    (fun t -> if t.kind = Text then None else Some (show t))
    (tokens sql)

let check name expected sql =
  Alcotest.(check (list string)) name expected (marks sql)

let test_placeholders () =
  check "numbered, question, named"
    [ "$1"; "?"; ":name"; "$12" ]
    "a = $1 AND b = ? AND c = :name AND d = $12";
  check "a cast is not a name" [ ":p" ] "SELECT x::text, :p::int";
  check ":name? is a name, then a ?" [ ":name"; "?" ] "x = :name?";
  check "a colon alone, a slice of numbers" [] "a[1:2] : b";
  check "$ alone, and a number too long to be one" [] "$ $99999999999999999999";
  check "inside a word" [ "$1" ] "ab$1"

let test_quoted () =
  check "a string" [ "quoted 'a ? $1 :x'"; "$2" ] "'a ? $1 :x' $2";
  check "a doubled quote stays in strings"
    [ "quoted 'it'"; "quoted 's ?'"; "?" ]
    "'it''s ?' ?";
  check "a quoted name" [ {|quoted "a?b"|}; "?" ] {|"a?b" ?|};
  check "an E string and its escapes"
    [ {|quoted E'it\'s ?'|}; "?" ]
    {|E'it\'s ?' ?|};
  check "an E string with doubled quotes stays escaped"
    [ {|quoted E'it''s \'?'|}; "?" ]
    {|E'it''s \'?' ?|};
  check "E at the end of a word starts no escapes"
    [ {|quoted 'a\'|}; "?"; "quoted '" ]
    {|nameE'a\' ?'|};
  check "a $$ body" [ "quoted $$ ? $1 :x ' $$"; ":y" ] "$$ ? $1 :x ' $$ :y";
  check "a tagged body ends at its own tag"
    [ "quoted $fn$ a $$ b $fn$"; "?" ]
    "$fn$ a $$ b $fn$ ?";
  check "unclosed: to the end" [ "quoted 'a ? " ] "x 'a ? ";
  check "unclosed body" [ "quoted $$ ? " ] "$$ ? "

let test_comments () =
  check "a line comment ends at the newline"
    [ "comment -- ? $1 :x"; "?" ]
    "a -- ? $1 :x\n?";
  check "a block comment" [ "comment /* ? \n :x */"; ":y" ] "/* ? \n :x */ :y";
  check "nested block comments"
    [ "comment /* outer /* inner ? */ :x */"; "?" ]
    "/* outer /* inner ? */ :x */ ?";
  check "unclosed nested block comment"
    [ "comment /* outer /* inner */ ?" ]
    "/* outer /* inner */ ?";
  check "a minus, a slash" [ "?" ] "a - b / c ?";
  check "a quote in a comment opens nothing" [ "comment -- it's"; "?" ]
    "-- it's\n?"

(* Nothing is lost or moved: the texts, joined, are the input, and each
   offset is where its text is. *)
let test_whole () =
  List.iter
    (fun sql ->
      let toks = tokens sql in
      Alcotest.(check string)
        "joined" sql
        (String.concat "" (List.map (fun t -> t.text) toks));
      List.iter
        (fun t ->
          Alcotest.(check string)
            "at its offset" t.text
            (String.sub sql t.at (String.length t.text)))
        toks)
    [
      "";
      "SELECT 1";
      "SELECT a, 'b''c', \"d\" FROM t -- x\nWHERE e = $1 AND f ? :g /* h */";
      "'";
      "$";
      "$a";
      ":";
      "E'\\";
      "/*";
      "--";
      "été = :é";
    ]

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "sql_tokens"
    [
      ( "tokens",
        [
          t "placeholders" test_placeholders;
          t "quoted" test_quoted;
          t "comments" test_comments;
          t "nothing lost" test_whole;
        ] );
    ]
