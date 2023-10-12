[@deriving show({with_path: false})]
type token =
  | EOF
  | IDENT(string) // <ident-token>
  | BAD_IDENT // TODO: this is needed?
  | FUNCTION(string) // <function-token>
  | AT_KEYWORD(string) // <at-keyword-token>
  | HASH(string, [ | `ID | `UNRESTRICTED]) // <hash-token>
  | STRING(string) // <string-token>
  | BAD_STRING(string) // <bad-string-token>
  | URL(string) // <url-token>
  | BAD_URL // <bad-url-token>
  | DELIM(string) // <delim-token>
  | NUMBER(float) // <number-token>
  | PERCENTAGE(float) // <percentage-token>
  | DIMENSION(float, string) // <dimension-token>
  | WS // <whitespace-token>
  | CDO // <CDO-token>
  | CDC // <CDC-token>
  | COLON // <colon-token>
  | SEMI_COLON // <semicolon-token>
  | COMMA // <comma-token>
  | LEFT_BRACKET // <[-token>
  | RIGHT_BRACKET // <]-token>
  | LEFT_PAREN // <(-token>
  | RIGHT_PAREN // <)-token>
  | LEFT_BRACE // <{-token>
  | RIGHT_BRACE // <}-token>
  | DOUBLE_COLON // :
  | PERCENT // %
  | AMPERSAND // &
  | IMPORTANT // !important
  | TAG(string)
  | OPERATOR(string)
  | COMBINATOR(string)
  | AT_MEDIA(string)
  | AT_KEYFRAMES(string)
  | AT_RULE_STATEMENT(string)
  | AT_RULE(string)
  | HASH_(string)
  | NUMBER_(string)
  | UNICODE_RANGE(string)
  | FLOAT_DIMENSION((string, string))
  | DIMENSION_((string, string))
  | INTERPOLATION(list(string))
  | DOT
  | ASTERISK
  | NTH_FUNCTION(string);

let string_of_char = c => String.make(1, c);

let humanize =
  fun
  | EOF => "the end"
  | IDENT(str) => "ident " ++ str
  | BAD_IDENT => "bad  ident"
  | FUNCTION(f) => "function " ++ f
  | AT_KEYWORD(at) => "@ " ++ at
  | HASH(h, _) => "hash: #" ++ h
  | STRING(s) => {|string "|} ++ s ++ {|"|}
  | BAD_STRING(_) => "bad string"
  | URL(u) => "url " ++ u
  | BAD_URL => "bad url"
  | DELIM(d) => "delimiter " ++ d
  | NUMBER(f) => "number: " ++ string_of_float(f)
  | PERCENTAGE(f) =>
    "percentage: " ++ string_of_float(f) ++ string_of_char('%')
  | DIMENSION(f, s) => "dimension: " ++ string_of_float(f) ++ s
  | WS => "whitespace"
  | CDO => "<!--"
  | CDC => "-->"
  | COLON => ":"
  | SEMI_COLON => ";"
  | COMMA => ","
  | LEFT_BRACKET => "["
  | RIGHT_BRACKET => "]"
  | LEFT_PAREN => "("
  | RIGHT_PAREN => ")"
  | LEFT_BRACE => "{"
  | RIGHT_BRACE => "}"
  | DOUBLE_COLON => "DOUBLE_COLON"
  | PERCENT => "PERCENT"
  | AMPERSAND => "AMPERSAND"
  | IMPORTANT => "IMPORTANT"
  | TAG(s) => "TAG('" ++ s ++ "')"
  | OPERATOR(s) => "OPERATOR('" ++ s ++ "')"
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
  | ASTERISK => "ASTERISK"
  | NTH_FUNCTION(fn) => "FUNCTION(" ++ fn ++ ")";

type error =
  | Invalid_code_point
  | Eof
  | New_line;

let show_error =
  fun
  | Invalid_code_point => "Invalid code point"
  | Eof => "Unexpected end"
  | New_line => "New line";
