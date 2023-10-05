include Ast;
module Tokens = Tokens
// module Lexer = Lexer

module Location = Ppxlib.Location;

let provider = (buf, ()) => {
  let token = Lexer.tokenizer(buf);
  let (start, stop) = Sedlexing.lexing_positions(buf);
  (token, start, stop);
};

let multiplier_of_lex =
  MenhirLib.Convert.Simplified.traditional2revised(Parser.multiplier_of_lex);

let rec string_of_multiplier =
  fun
  | One => ""
  | Zero_or_more => "*"
  | One_or_more => "+"
  | Optional => "?"
  | Repeat(min, Some(max)) when min == max =>
    "{" ++ string_of_int(min) ++ "}"
  | Repeat(min, Some(max)) =>
    "{" ++ string_of_int(min) ++ "," ++ string_of_int(max) ++ "}"
  | Repeat(min, None) => "{" ++ string_of_int(min) ++ ",}"
  | Repeat_by_comma(1, None) => "#"
  | Repeat_by_comma(min, max) =>
    "#" ++ string_of_multiplier(Repeat(min, max))
  | At_least_one => "!";

let _multiplier_of_string = string =>
  Sedlexing.Utf8.from_string(string) |> provider |> multiplier_of_lex;

let value_of_lex =
  MenhirLib.Convert.Simplified.traditional2revised(Parser.value_of_lex);

// let stylesheet =
//   MenhirLib.Convert.Simplified.traditional2revised(Parser.stylesheet);
//   let keyframes =
//   MenhirLib.Convert.Simplified.traditional2revised(Parser.keyframes);
// let declaration =
//   MenhirLib.Convert.Simplified.traditional2revised(Parser.declaration);
//   let declaration_list =
//   MenhirLib.Convert.Simplified.traditional2revised(Parser.declaration_list);

let rec string_of_value = value => {
  let child_needs_brackets = (parent, child) => {
    let precedence =
      fun
      | Combinator(Static, _) => Some(0)
      | Combinator(And, _) => Some(1)
      | Combinator(Or, _) => Some(2)
      | Combinator(Xor, _) => Some(3)
      | _ => None;

    let parent = precedence(parent);
    let child = precedence(child);

    switch (parent, child) {
    | (_, None) => false
    | (None, _) => false
    | (Some(parent), Some(child)) => child > parent
    };
  };
  let child = child =>
    child_needs_brackets(value, child)
      ? "[ " ++ string_of_value(child) ++ " ]" : string_of_value(child);
  let childs = (sep, childs) =>
    childs |> List.map(child) |> String.concat(sep);
  let (string, multiplier) =
    switch (value) {
    | Terminal(kind, multiplier) =>
      let full_name =
        switch (kind) {
        | Delim(d) => {|'|} ++ d ++ {|'|}
        | Keyword(name) => {|'|} ++ name ++ {|'|}
        | Data_type(name) => "<" ++ name ++ ">"
        | Property_type(name) => "<'" ++ name ++ "'>"
        };
      (full_name, Some(multiplier));
    | Combinator(kind, values) =>
      let separator =
        switch (kind) {
        | Static => " "
        | And => " && "
        | Or => " || "
        | Xor => " | "
        };
      (childs(separator, values), None);
    | Group(value, multiplier) => (child(value), Some(multiplier))
    | Function_call(name, value) => (
        name ++ "( " ++ child(value) ++ " )",
        None,
      )
    };

  switch (value, multiplier) {
  | (Group(_), None) => "[ " ++ string ++ " ]"
  | (Group(_), Some(multiplier)) =>
    "[ " ++ string ++ " ]" ++ string_of_multiplier(multiplier)
  | (_, None)
  | (_, Some(One)) => string
  | (_, Some(multiplier)) =>
    "[ " ++ string ++ " ]" ++ string_of_multiplier(multiplier)
  };
};

exception ParseError(string);

let value_of_string = string =>
  try(Sedlexing.Utf8.from_string(string) |> provider |> value_of_lex) {
  | _ => raise(ParseError(string))
  };

type token_with_location = {
  txt: result(Tokens.token, (Tokens.token, Tokens.error)),
  loc: Location.t,
};

// TODO: that's definitly ugly
/* TODO: Use lex_buffer from parser to keep track of the file */
let from_string = string => {
  let buf = Sedlexing.Utf8.from_string(string);
  let rec read = acc => {
    let (loc_start, _) = Sedlexing.lexing_positions(buf);
    let value = Lexer.consume(buf);
    let (_, loc_end) = Sedlexing.lexing_positions(buf);

    let token_with_loc: token_with_location = {
      txt: value,
      loc: {
        loc_start,
        loc_end,
        loc_ghost: false,
      },
    };

    let acc = [token_with_loc, ...acc];
    switch (value) {
    | Ok(EOF) => Ok(acc)
    | _ when loc_start.pos_cnum == loc_end.pos_cnum => Error(`Frozen)
    | _ => read(acc)
    };
  };

  read([]);
};
