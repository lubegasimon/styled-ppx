/** CSS lexer
  * Reference:
  * https://www.w3.org/TR/css-syntax-3/ */
module Sedlexing = Lex_buffer;
// open Sedlexing;
// module Parser = Css_parser;
module Types = Css_types;
module Tokens = Tokens;
open Tokens;

/** Signals a lexing error at the provided source location. */
exception LexingError((Lexing.position, string));

/** Signals a parsing error at the provided token and its start and end
 * locations. */
// exception ParseError((Parser.token, Lexing.position, Lexing.position));

let unreachable = () =>
  failwith(
    "This match case is unreachable. sedlex needs a last case as wildcard _. If this error appears, means that there's a bug in the lexer.",
  );

/* TODO: Create a token file to export the tokens so that it doesn't appear like
   we are calling the parser via `Parser....` */
let token_to_string =
  fun
  | EOF => ""
  | LEFT_BRACE => "{"
  | RIGHT_BRACE => "}"
  | LEFT_PAREN => "("
  | RIGHT_PAREN => ")"
  | LEFT_BRACKET => "["
  | RIGHT_BRACKET => "]"
  | COLON => ":"
  | DOUBLE_COLON => "::"
  | SEMI_COLON => ";"
  | PERCENT => "%"
  | AMPERSAND => "&"
  | IMPORTANT => "!important"
  | IDENT(s) => s
  | TAG(s) => s
  | STRING(s) => "'" ++ s ++ "'"
  | OPERATOR(s) => s
  | COMBINATOR(s)
  | DELIM(s) => s
  | AT_MEDIA(s)
  | AT_KEYFRAMES(s)
  | AT_RULE_STATEMENT(s)
  | AT_RULE(s) => "@" ++ s
  | HASH_(s) => "#" ++ s
  | NUMBER_(s) => s
  | UNICODE_RANGE(s) => s
  | FLOAT_DIMENSION((n, s)) => n ++ s
  | DIMENSION_((n, d)) => n ++ d
  | INTERPOLATION(v) => String.concat(".", v)
  | WS => " "
  | DOT => "."
  | COMMA => ","
  | ASTERISK => "*"
  | FUNCTION(fn) => fn ++ "("
  | NTH_FUNCTION(fn) => fn ++ "("
  | URL(url) => url ++ "("
  | BAD_URL => "bad url"
  | BAD_IDENT => "bad indent"
  /*
   FIXME:
   (CDO|CDC|AT_KEYWORD _|HASH (_, _)|BAD_STRING _|NUMBER _|
   PERCENTAGE _|DIMENSION (_, _))
   */
  | _ => assert(false);

let token_to_debug =
  fun
  | EOF => "EOF"
  | LEFT_BRACE => "LEFT_BRACE"
  | RIGHT_BRACE => "RIGHT_BRACE"
  | LEFT_PAREN => "LEFT_PAREN"
  | RIGHT_PAREN => "RIGHT_PAREN"
  | LEFT_BRACKET => "LEFT_BRACKET"
  | RIGHT_BRACKET => "RIGHT_BRACKET"
  | COLON => "COLON"
  | DOUBLE_COLON => "DOUBLE_COLON"
  | SEMI_COLON => "SEMI_COLON"
  | PERCENT => "PERCENTAGE"
  | AMPERSAND => "AMPERSAND"
  | IMPORTANT => "IMPORTANT"
  | IDENT(s) => "IDENT('" ++ s ++ "')"
  | TAG(s) => "TAG('" ++ s ++ "')"
  | STRING(s) => "STRING('" ++ s ++ "')"
  | OPERATOR(s) => "OPERATOR('" ++ s ++ "')"
  | DELIM(s) => "DELIM('" ++ s ++ "')"
  | AT_RULE(s) => "AT_RULE('" ++ s ++ "')"
  | AT_RULE_STATEMENT(s) => "AT_RULE_STATEMENT('" ++ s ++ "')"
  | AT_MEDIA(s) => "AT_MEDIA('" ++ s ++ "')"
  | AT_KEYFRAMES(s) => "AT_KEYFRAMES('" ++ s ++ "')"
  | HASH_(s) => "HASH('" ++ s ++ "')"
  | NUMBER_(s) => "NUMBER('" ++ s ++ "')"
  | UNICODE_RANGE(s) => "UNICODE_RANGE('" ++ s ++ "')"
  | FLOAT_DIMENSION((n, s)) => "FLOAT_DIMENSION('" ++ n ++ ", " ++ s ++ "')"
  | DIMENSION_((n, d)) => "DIMENSION('" ++ n ++ ", " ++ d ++ "')"
  | INTERPOLATION(v) => "VARIABLE('" ++ String.concat(".", v) ++ "')"
  | COMBINATOR(s) => "COMBINATOR(" ++ s ++ ")"
  | DOT => "DOT"
  | COMMA => "COMMA"
  | WS => "WS"
  | ASTERISK => "ASTERISK"
  | FUNCTION(fn) => "FUNCTION(" ++ fn ++ ")"
  | NTH_FUNCTION(fn) => "FUNCTION(" ++ fn ++ ")"
  | URL(u) => "URL(" ++ u ++ ")"
  | BAD_URL => "BAD_URL"
  // FIXME:
  | _ => assert(false);

/* Regexes */
let newline = [%sedlex.regexp? '\n' | "\r\n" | '\r' | '\012'];

let whitespace = [%sedlex.regexp? " " | '\t' | newline];

let whitespaces = [%sedlex.regexp? Star(whitespace)];

let digit = [%sedlex.regexp? '0' .. '9'];

let non_ascii = [%sedlex.regexp? '\160' .. '\255'];

let up_to_6_hex_digits = [%sedlex.regexp? Rep(hex_digit, 1 .. 6)];

let unicode = [%sedlex.regexp? ('\\', up_to_6_hex_digits, Opt(whitespace))];

let unicode_range = [%sedlex.regexp?
  Rep(hex_digit | '?', 1 .. 6) |
  (up_to_6_hex_digits, '-', up_to_6_hex_digits)
];

let escape = [%sedlex.regexp?
  unicode | ('\\', Compl('\r' | '\n' | '\012' | hex_digit))
];

let ident_start = [%sedlex.regexp?
  '_' | 'a' .. 'z' | 'A' .. 'Z' | '$' | non_ascii | escape
];

let ident_char = [%sedlex.regexp?
  '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | non_ascii | escape
];

let ident = [%sedlex.regexp?
  (Opt('-'), Opt('-'), ident_start, Star(ident_char))
];

let variable_ident_char = [%sedlex.regexp?
  '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | non_ascii | escape | '\''
];
let variable_name = [%sedlex.regexp? Star(variable_ident_char)];
let module_variable = [%sedlex.regexp? (variable_name, '.')];
let variable = [%sedlex.regexp?
  ('$', '(', Opt(Star(module_variable)), variable_name, ')')
];

let string_quote = [%sedlex.regexp?
  (
    '"',
    Star(Compl('\n' | '\r' | '\012' | '"') | ('\\', newline) | escape),
    '"',
  )
];

let string_apos = [%sedlex.regexp?
  (
    '\'',
    Star(Compl('\n' | '\r' | '\012' | '\'') | ('\\', newline) | escape),
    '\'',
  )
];

let string = [%sedlex.regexp? string_quote | string_apos];

let name = [%sedlex.regexp? Plus(ident_char)];

let number = [%sedlex.regexp?
  (
    Opt('-'),
    Plus(digit),
    Opt('.', Plus(digit)),
    Opt('e' | 'E', '+' | '-', Plus(digit)),
  ) |
  (Opt('-'), '.', Plus(digit), Opt('e' | 'E', '+' | '-', Plus(digit)))
];

let non_printable = [%sedlex.regexp?
  '\000' .. '\b' | '\011' | '\014' .. '\031' | '\127'
];

let operator = [%sedlex.regexp? "~=" | "|=" | "^=" | "$=" | "*=" | "="];

let combinator = [%sedlex.regexp? '>' | '+' | '~' | "||"];

let at_rule_without_body = [%sedlex.regexp?
  ("@", "charset" | "import" | "namespace")
];
let at_rule = [%sedlex.regexp? ("@", ident)];
let at_media = [%sedlex.regexp? ("@", "media")];
let at_keyframes = [%sedlex.regexp? ("@", "keyframes")];

let is_tag =
  fun
  | "a"
  | "abbr"
  | "address"
  | "area"
  | "article"
  | "aside"
  | "audio"
  | "b"
  | "base"
  | "bdi"
  | "bdo"
  | "blockquote"
  | "body"
  | "br"
  | "button"
  | "canvas"
  | "caption"
  | "cite"
  | "code"
  | "col"
  | "colgroup"
  | "data"
  | "datalist"
  | "dd"
  | "del"
  | "details"
  | "dfn"
  | "dialog"
  | "div"
  | "dl"
  | "dt"
  | "em"
  | "embed"
  | "fieldset"
  | "figcaption"
  | "figure"
  | "footer"
  | "form"
  | "h1"
  | "h2"
  | "h3"
  | "h4"
  | "h5"
  | "h6"
  | "head"
  | "header"
  | "hgroup"
  | "hr"
  | "html"
  | "i"
  | "iframe"
  | "img"
  | "input"
  | "ins"
  | "kbd"
  | "label"
  | "legend"
  | "li"
  | "link"
  | "main"
  | "map"
  | "mark"
  | "math"
  | "menu"
  | "menuitem"
  | "meta"
  | "meter"
  | "nav"
  | "noscript"
  | "object"
  | "ol"
  | "optgroup"
  | "option"
  | "output"
  | "p"
  | "param"
  | "picture"
  | "pre"
  | "progress"
  | "q"
  | "rb"
  | "rp"
  | "rt"
  | "rtc"
  | "ruby"
  | "s"
  | "samp"
  | "script"
  | "section"
  | "select"
  | "slot"
  | "small"
  | "source"
  | "span"
  | "strong"
  | "style"
  | "sub"
  | "summary"
  | "sup"
  | "svg"
  | "table"
  | "tbody"
  | "td"
  | "template"
  | "textarea"
  | "tfoot"
  | "th"
  | "thead"
  | "time"
  | "title"
  | "tr"
  | "track"
  | "u"
  | "ul"
  | "var"
  | "video"
  | "wbr" => true
  | _ => false;

let _a = [%sedlex.regexp? 'A' | 'a'];
let _b = [%sedlex.regexp? 'B' | 'b'];
let _c = [%sedlex.regexp? 'C' | 'c'];
let _d = [%sedlex.regexp? 'D' | 'd'];
let _e = [%sedlex.regexp? 'E' | 'e'];
let _f = [%sedlex.regexp? 'F' | 'f'];
let _g = [%sedlex.regexp? 'G' | 'g'];
let _h = [%sedlex.regexp? 'H' | 'h'];
let _i = [%sedlex.regexp? 'I' | 'i'];
let _j = [%sedlex.regexp? 'J' | 'j'];
let _k = [%sedlex.regexp? 'K' | 'k'];
let _l = [%sedlex.regexp? 'L' | 'l'];
let _m = [%sedlex.regexp? 'M' | 'm'];
let _n = [%sedlex.regexp? 'N' | 'n'];
let _o = [%sedlex.regexp? 'O' | 'o'];
let _p = [%sedlex.regexp? 'P' | 'p'];
let _q = [%sedlex.regexp? 'Q' | 'q'];
let _r = [%sedlex.regexp? 'R' | 'r'];
let _s = [%sedlex.regexp? 'S' | 's'];
let _t = [%sedlex.regexp? 'T' | 't'];
let _u = [%sedlex.regexp? 'U' | 'u'];
let _v = [%sedlex.regexp? 'V' | 'v'];
let _w = [%sedlex.regexp? 'W' | 'w'];
let _x = [%sedlex.regexp? 'X' | 'x'];
let _y = [%sedlex.regexp? 'Y' | 'y'];
let _z = [%sedlex.regexp? 'Z' | 'z'];

let important = [%sedlex.regexp?
  ("!", whitespaces, _i, _m, _p, _o, _r, _t, _a, _n, _t)
];

let length = [%sedlex.regexp?
  (_c, _a, _p) | (_c, _h) | (_e, _m) | (_e, _x) | (_i, _c) | (_l, _h) |
  (_r, _e, _m) |
  (_r, _l, _h) |
  (_v, _h) |
  (_v, _w) |
  (_v, _i) |
  (_v, _b) |
  (_v, _m, _i, _n) |
  (_v, _m, _a, _x) |
  (_c, _m) |
  (_m, _m) |
  _q |
  (_i, _n) |
  (_p, _c) |
  (_p, _t) |
  (_p, _x) |
  (_f, _r)
];

let angle = [%sedlex.regexp?
  (_d, _e, _g) | (_g, _r, _a, _d) | (_r, _a, _d) | (_t, _u, _r, _n)
];

let time = [%sedlex.regexp? _s | (_m, _s)];

let frequency = [%sedlex.regexp? (_h, _z) | (_k, _h, _z)];

let hex_digit = [%sedlex.regexp? digit | 'A' .. 'F' | 'a' .. 'f'];

let non_ascii_code_point = [%sedlex.regexp? Sub(any, '\000' .. '\128')]; // greater than \u0080

let identifier_start_code_point = [%sedlex.regexp?
  'a' .. 'z' | 'A' .. 'Z' | non_ascii | non_ascii_code_point | '_'
];
let starts_with_a_valid_escape = [%sedlex.regexp? ('\\', Sub(any, '\n'))];
let starts_an_identifier = [%sedlex.regexp?
  ('-', '-' | identifier_start_code_point | starts_with_a_valid_escape) |
  identifier_start_code_point
];
/* Added "'" to identifier to enable Language Variables */
let identifier_code_point = [%sedlex.regexp?
  identifier_start_code_point | digit | '-' | "'"
];
let non_printable_code_point = [%sedlex.regexp?
  '\000' .. '\b' | '\011' | '\014' .. '\031' | '\127'
];

let starts_an_identifier = [%sedlex.regexp?
  ('-', '-' | identifier_start_code_point | starts_with_a_valid_escape) |
  identifier_start_code_point
];

let starts_a_number = [%sedlex.regexp?
  ("+" | "-", digit) | ("+" | "-", ".", digit) | ('.', digit)
];

// FIXME: Consider moving this back to Tokenizer
let (let.ok) = Result.bind;

/* This module is a copy/paste of Reason_css_lexer in favor of moving everything into css_lexer */
module Tokenizer = {
  // open Reason_css_lexer;
  let lexeme = Sedlexing.utf8;

  /* I don't think this function in any way calls the Parser, not a lexer.
     What it does is defines a lexing function that consumes white zero or more
     white space and produces a corresponding token `WS`. Parser, could be replaced
     with Token so that it doesn't turn out confusing that the function is calling
     a Parser. By definition, the process is like;
     parser |> lexer tokens */
  let consume_whitespace = buf =>
    switch%sedlex (buf) {
    | Star(whitespace) => WS
    | _ => WS
    };

  let string_of_uchar = char => {
    let buf = Buffer.create(0);
    Buffer.add_utf_8_uchar(buf, char);
    Buffer.contents(buf);
  };

  let uchar_of_int = n => Uchar.of_int(n) |> string_of_uchar;
  let is_surrogate = char_code => char_code >= 0xD800 && char_code <= 0xDFFF;

  let check = (f, buf) => {
    // TODO: why this second int?
    Sedlexing.mark(buf, 0);
    let value = f(buf);
    let _ = Sedlexing.backtrack(buf);
    value;
  };

  // https://drafts.csswg.org/css-syntax-3/#consume-an-escaped-code-point
  let consume_escaped = buf => {
    switch%sedlex (buf) {
    // TODO: spec typo? No more than 5?
    | Rep(hex_digit, 1 .. 6) =>
      let hex_string = "0x" ++ lexeme(buf);
      let char_code = int_of_string(hex_string);
      let char = uchar_of_int(char_code);
      let _ = consume_whitespace(buf);
      char_code == 0 || is_surrogate(char_code)
        ? Error((Uchar.rep, Invalid_code_point)) : Ok(char);
    | eof => Error((Uchar.rep, Eof))
    | any => Ok(lexeme(buf))
    | _ => unreachable()
    };
  };

  // https://drafts.csswg.org/css-syntax-3/#consume-remnants-of-bad-url
  let rec consume_remnants_bad_url = buf =>
    switch%sedlex (buf) {
    | ")"
    | eof => ()
    | escape =>
      let _ = consume_escaped(buf);
      consume_remnants_bad_url(buf);
    | any => consume_remnants_bad_url(buf)
    | _ => unreachable()
    };

  // https://drafts.csswg.org/css-syntax-3/#consume-url-token
  let consume_url = buf => {
    let _ = consume_whitespace(buf);
    let rec read = acc => {
      let when_whitespace = () => {
        let _ = consume_whitespace(buf);
        switch%sedlex (buf) {
        | ')' => Ok(URL(acc))
        | eof => Error((URL(acc), Eof))
        | _ =>
          consume_remnants_bad_url(buf);
          Ok(BAD_URL);
        };
      };
      switch%sedlex (buf) {
      | ')' => Ok(URL(acc))
      | eof => Error((URL(acc), Eof))
      | whitespace => when_whitespace()
      | '"'
      | '\''
      | '('
      | non_printable_code_point =>
        consume_remnants_bad_url(buf);
        // TODO: location on error
        Error((BAD_URL, Invalid_code_point));
      | escape =>
        switch (consume_escaped(buf)) {
        | Ok(char) => read(acc ++ char)
        | Error((_, error)) => Error((BAD_URL, error))
        }
      | any => read(acc ++ lexeme(buf))
      | _ => unreachable()
      };
    };
    read(lexeme(buf));
  };

  // https://drafts.csswg.org/css-syntax-3/#consume-name
  let consume_identifier = buf => {
    let rec read = acc =>
      switch%sedlex (buf) {
      | identifier_code_point => read(acc ++ lexeme(buf))
      | escape =>
        // TODO: spec, what should happen when fails?
        let.ok char = consume_escaped(buf);
        read(acc ++ char);
      | _ => Ok(acc)
      };
    read(lexeme(buf));
  };

  let handle_consume_identifier =
    fun
    | Error((_, error)) => Error((BAD_IDENT, error))
    | Ok(string) => Ok(string);

  let consume_function = string => {
    switch (string) {
    | "nth-last-child"
    | "nth-child"
    | "nth-of-type"
    | "nth-last-of-type" => NTH_FUNCTION(string)
    | _ => FUNCTION(string)
    };
  };

  // https://drafts.csswg.org/css-syntax-3/#consume-ident-like-token
  let consume_ident_like = buf => {
    let read_url = string => {
      // TODO: the whitespace trickery here?
      let _ = consume_whitespace(buf);
      let is_function =
        check(buf =>
          switch%sedlex (buf) {
          | '\''
          | '"' => true
          | _ => false
          }
        );
      is_function(buf) ? Ok(consume_function(string)) : consume_url(buf);
    };

    let.ok string = consume_identifier(buf) |> handle_consume_identifier;
    switch%sedlex (buf) {
    | "(" =>
      switch (string) {
      | "url" => read_url(string)
      | _ => Ok(consume_function(string))
      }
    | _ => is_tag(string) ? Ok(TAG(string)) : Ok(IDENT(string))
    };
  };

  // https://drafts.csswg.org/css-syntax-3/#starts-with-a-valid-escape
  /* FIXME: Move to notes: A code point is a Unicode code point and is represented as "U+" followed by
     four-to-six ASCII upper hex digits, in the range U+0000 to U+10FFFF, inclusive */
  let check_if_two_code_points_are_a_valid_escape = buf =>
    switch%sedlex (buf) {
    | ("\\", '\n') => false
    | ("\\", any) => true
    | _ => false
    };

  // https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
  let check_if_three_codepoints_would_start_an_identifier = buf =>
    switch%sedlex (buf) {
    | ('-', identifier_start_code_point | '-') => true
    // TODO: test the code_points case
    | '-' => check_if_two_code_points_are_a_valid_escape(buf)
    | identifier_start_code_point => true
    | _ => check_if_two_code_points_are_a_valid_escape(buf)
    };

  // https://drafts.csswg.org/css-syntax-3/#starts-with-a-number
  let check_if_three_code_points_would_start_a_number = buf =>
    switch%sedlex (buf) {
    | ("+" | "-", digit)
    | ("+" | "-", ".", digit) => true
    | ('.', digit) => true
    | _ => false
    };

  let check_if_three_codepoints_would_start_an_identifier =
    check(check_if_three_codepoints_would_start_an_identifier);
  let check_if_three_code_points_would_start_a_number =
    check(check_if_three_code_points_would_start_a_number);

  // https://drafts.csswg.org/css-syntax-3/#consume-string-token
  // TODO: when EOF is bad-string-token or string-token
  // TODO: currently it is a little bit different than the specification
  let consume_string = (ending_code_point, buf) => {
    let rec read = acc => {
      switch%sedlex (buf) {
      | '\''
      | '"' =>
        let code_point = lexeme(buf);
        code_point == ending_code_point
          ? Ok(acc) : read(acc ++ lexeme(buf));
      | eof => Error((acc, Eof))
      | newline => Error((acc, New_line))
      | escape =>
        switch (consume_escaped(buf)) {
        | Ok(char) => read(acc ++ char)
        | Error((_, error)) => Error((acc, error))
        }
      | any => read(acc ++ lexeme(buf))
      | _ => unreachable()
      };
    };
    // FIXME: Review
    // read (lexeme(buf))
    switch (read("")) {
    | Ok(string) => Ok(STRING(string))
    | Error((string, error)) => Error((BAD_STRING(string), error))
    };
  };

  let consume_number = buf => {
    let append = repr => repr ++ lexeme(buf);

    let kind = `Integer; // 1
    let repr = "";
    let repr =
      switch%sedlex (buf) {
      | (Opt("+" | "-"), Plus(digit)) => append(repr)
      | _ => repr
      }; // 2 - 3
    let (kind, repr) =
      switch%sedlex (buf) {
      | (".", Plus(digit)) => (`Number, append(repr))
      | _ => (kind, repr)
      }; // 4
    let (kind, repr) =
      switch%sedlex (buf) {
      | ('E' | 'e', Opt('+' | '-'), Plus(digit)) => (
          `Number,
          append(repr),
        )
      | _ => (kind, repr)
      }; // 5
    let value = float_of_string(repr); // 6
    (value, kind); // 7
  };

  // https://drafts.csswg.org/css-syntax-3/#consume-numeric-token
  let consume_numeric = buf => {
    // TODO: kind matters?
    let (number, _kind) = consume_number(buf);
    if (check_if_three_codepoints_would_start_an_identifier(buf)) {
      // TODO: should it be BAD_IDENT?
      let.ok string = consume_identifier(buf) |> handle_consume_identifier;
      Ok(DIMENSION(number, string));
    } else {
      switch%sedlex (buf) {
      | '%' => Ok(PERCENTAGE(number))
      | _ => Ok(NUMBER(number))
      };
    };
  };

  // https://drafts.csswg.org/css-syntax-3/#consume-comment
  let consume_comment = buf => {
    let rec read_until_closes = () =>
      switch%sedlex (buf) {
      | "*/" => Ok()
      | eof => Error(((), Eof))
      | _ => read_until_closes()
      };
    switch%sedlex (buf) {
    | "/*" => read_until_closes()
    | _ => Ok()
    };
  };

  let consume = buf => {
    let consume_hash = () =>
      switch%sedlex (buf) {
      | identifier_code_point
      | starts_with_a_valid_escape =>
        Sedlexing.rollback(buf);
        switch%sedlex (buf) {
        | identifier_start_code_point =>
          Sedlexing.rollback(buf);
          let.ok string =
            consume_identifier(buf) |> handle_consume_identifier;
          Ok(HASH(string, `ID));
        | _ =>
          let.ok string =
            consume_identifier(buf) |> handle_consume_identifier;
          Ok(HASH(string, `UNRESTRICTED));
        };
      | _ => Ok(DELIM("#"))
      };
    let consume_minus = () =>
      switch%sedlex (buf) {
      | starts_a_number =>
        Sedlexing.rollback(buf);
        consume_numeric(buf);
      | "-->" => Ok(CDC)
      | starts_an_identifier =>
        Sedlexing.rollback(buf);
        consume_ident_like(buf);
      | _ =>
        let _ = Sedlexing.next(buf);
        Ok(DELIM("-"));
      };

    switch%sedlex (buf) {
    | whitespace => Ok(consume_whitespace(buf))
    | "\"" => consume_string("\"", buf)
    | "#" => consume_hash()
    | "'" => consume_string("'", buf)
    | "(" => Ok(LEFT_PAREN)
    | ")" => Ok(RIGHT_PAREN)
    | "+" =>
      let _ = Sedlexing.backtrack(buf);
      if (check_if_three_code_points_would_start_a_number(buf)) {
        consume_numeric(buf);
      } else {
        let _ = Sedlexing.next(buf);
        Ok(DELIM("+"));
      };
    | "," => Ok(COMMA)
    | "-" =>
      Sedlexing.rollback(buf);
      consume_minus();
    | "." =>
      let _ = Sedlexing.backtrack(buf);
      if (check_if_three_code_points_would_start_a_number(buf)) {
        consume_numeric(buf);
      } else {
        let _ = Sedlexing.next(buf);
        Ok(DELIM("."));
      };
    | ":" => Ok(COLON)
    | ";" => Ok(SEMI_COLON)
    | "<!--" => Ok(CDO)
    | "<" => Ok(DELIM("<"))
    | "@" =>
      if (check_if_three_codepoints_would_start_an_identifier(buf)) {
        // TODO: grr BAD_IDENT
        let.ok string = consume_identifier(buf) |> handle_consume_identifier;
        Ok(AT_KEYWORD(string));
      } else {
        Ok(DELIM("@"));
      }
    | "[" => Ok(LEFT_BRACKET)
    | "\\" =>
      Sedlexing.rollback(buf);
      switch%sedlex (buf) {
      | starts_with_a_valid_escape =>
        Sedlexing.rollback(buf);
        consume_ident_like(buf);
      // TODO: this error should be different
      | _ => Error((DELIM("/"), Invalid_code_point))
      };
    | "]" => Ok(RIGHT_BRACKET)
    | digit =>
      let _ = Sedlexing.backtrack(buf);
      consume_numeric(buf);
    | identifier_start_code_point =>
      let _ = Sedlexing.backtrack(buf);
      consume_ident_like(buf);
    | eof => Ok(EOF)
    | any => Ok(DELIM(lexeme(buf)))
    | _ =>
      failwith(
        "This match case is unreachable. sedlex needs a last case as wildcard _. If this error appears, means that there's a bug in the lexer.",
      )
    };
  };
};

let handle_tokenizer_error = (buf: Sedlexing.t) =>
  fun
  | Ok(value) => value
  | Error((_, msg)) => {
      let error: string = show_error(msg);
      let position = buf.pos;
      raise @@ LexingError((position, error));
    };

let skip_whitespace = ref(false);

let rec get_next_token = buf => {
  open Sedlexing; // open Parser;

  switch%sedlex (buf) {
  | eof => EOF
  | "/*" => discard_comments(buf)
  | '.' => DOT
  | ';' => SEMI_COLON
  | '}' =>
    skip_whitespace.contents = false;
    RIGHT_BRACE;
  | '{' =>
    skip_whitespace.contents = true;
    LEFT_BRACE;
  | "::" => DOUBLE_COLON
  | ':' => COLON
  | '(' => LEFT_PAREN
  | ')' => RIGHT_PAREN
  | '[' => LEFT_BRACKET
  | ']' => RIGHT_BRACKET
  | '%' => PERCENT
  | '&' =>
    skip_whitespace.contents = false;
    AMPERSAND;
  | '*' => ASTERISK
  | ',' => COMMA
  | variable =>
    // FIXME: To interpolation
    INTERPOLATION(
      latin1(~skip=2, ~drop=1, buf) |> String.split_on_char('.'),
    )
  | operator => OPERATOR(latin1(buf))
  | combinator => COMBINATOR(latin1(buf))
  | string => STRING(latin1(~skip=1, ~drop=1, buf))
  | important => IMPORTANT
  | at_media =>
    skip_whitespace.contents = false;
    AT_MEDIA(latin1(~skip=1, buf));
  | at_keyframes =>
    skip_whitespace.contents = false;
    AT_KEYFRAMES(latin1(~skip=1, buf));
  | at_rule =>
    skip_whitespace.contents = false;
    AT_RULE(latin1(~skip=1, buf));
  | at_rule_without_body =>
    skip_whitespace.contents = false;
    AT_RULE_STATEMENT(latin1(~skip=1, buf));
  /* NOTE: should be placed above ident, otherwise pattern with
   * '-[0-9a-z]{1,6}' cannot be matched */
  | (_u, '+', unicode_range) => UNICODE_RANGE(latin1(buf))
  | ('#', name) => HASH_(latin1(~skip=1, buf))
  | number => get_dimension(latin1(buf), buf)
  | whitespaces =>
    if (skip_whitespace^) {
      get_next_token(buf);
    } else {
      WS;
    }
  /* | "n" => N */
  | ("-", ident) => IDENT(latin1(buf))
  /* --variable */
  | ("-", "-", ident) => IDENT(latin1(buf))
  | identifier_start_code_point =>
    let _ = Sedlexing.backtrack(buf);
    Tokenizer.consume_ident_like(buf) |> handle_tokenizer_error(buf);
  | any => DELIM(latin1(buf))
  | _ => assert(false)
  };
}
and get_dimension = (n, buf) => {
  open Sedlexing;
  switch%sedlex (buf) {
  | length => FLOAT_DIMENSION((n, latin1(buf)))
  | angle => FLOAT_DIMENSION((n, latin1(buf)))
  | time => FLOAT_DIMENSION((n, latin1(buf)))
  | frequency => FLOAT_DIMENSION((n, latin1(buf)))
  | 'n' => DIMENSION_((n, latin1(buf)))
  | _ => NUMBER_(n)
  };
}
and discard_comments = buf => {
  switch%sedlex (buf) {
  | "*/" => get_next_token(buf)
  | any => discard_comments(buf)
  | eof =>
    raise(
      LexingError((buf.pos, "Unterminated comment at the end of the string")),
    )
  | _ => assert(false)
  };
};

let get_next_tokens_with_location = buf => {
  let (_, position_end) = Lex_buffer.lexing_positions(buf);
  let token = get_next_token(buf);
  let (_, position_end_after) = Lex_buffer.lexing_positions(buf);

  (token, position_end, position_end_after);
};

type token_with_location = {
  txt: result(Tokens.token, (Tokens.token, Tokens.error)),
  loc: Ppxlib.Location.t,
};

// TODO: that's definitly ugly
/* TODO: Use lex_buffer from parser to keep track of the file */
let from_string = string => {
  let buf = Sedlexing.from_string(string);
  let rec read = acc => {
    let (loc_start, _) = Sedlexing.lexing_positions(buf);
    let value = Tokenizer.consume(buf);
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
