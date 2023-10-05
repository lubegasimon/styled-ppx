[@deriving show]
type token =
/*
    TODO: Some comments are not necessary because tokens are
    self descrbing
*/
    // literals
    | LITERAL(string) // auto 'auto'
    | DATA(string) // <number>
    | PROPERTY(string) // <'color'>
    | STRING(string) // string
    | BAD_STRING(string) // bad-string
    | URL(string) // url
    | IDENT(string) // ident
    | BAD_IDENT // TODO: this is needed?
    | AT_KEYWORD(string) // @<keyword>
    | HASH(string, [ | `ID | `UNRESTRICTED]) // hash
    | BAD_URL // bad-url
    | NUMBER(float) // number
    | DELIM(string) // delim
    | DIMENSION(float, string) // dimension
    // combinators
    | DOUBLE_AMPERSAND // &&
    | DOUBLE_BAR // ||
    | BAR // |
    | LEFT_BRACKET // [
    | RIGHT_BRACKET // ]
    | LEFT_CURLY // {
    | RIGHT_CURLY // }
    | COMMA // ,
    | COLON // :
    | SEMICOLON // ;
    // modifiers
    | ASTERISK // *
    | PLUS // +
    | QUESTION_MARK // ?
    | RANGE(([ | `Comma | `Space], int, option(int))) // {1} {1,} {1, 2} #{1}
    | EXCLAMATION_POINT // !
    | PERCENTAGE(float) // %
    // for functions
    | FUNCTION(string) // function
    | LEFT_PARENS // (
    | RIGHT_PARENS // )
    // for required chars
    | CHAR(string) // ','
    // others
    | CDO // <CDO-token>
    | CDC // <CDC-token>
    // End
    | WHITESPACE // whitespace
    | EOF;
  
  

let string_of_char = c => String.make(1, c);

let humanize =
  fun
  | EOF => "the end"
  | IDENT(str) => "ident " ++ str
  | BAD_IDENT => "bad ident"
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
  | WHITESPACE => "whitespace"
  | CDO => "<!--"
  | CDC => "-->"
  | COLON => ":"
  | SEMICOLON => ";"
  | COMMA => ","
  | LEFT_BRACKET => "["
  | RIGHT_BRACKET => "]"
  | LEFT_PARENS => "("
  | RIGHT_PARENS => ")"
  | LEFT_CURLY => "{"
  | RIGHT_CURLY => "}"
  | DOUBLE_AMPERSAND => "&&"
  | DOUBLE_BAR => "||"
  | BAR => "|"
  | ASTERISK => "*"
  | PLUS => "+"
  | QUESTION_MARK => "?"
  | EXCLAMATION_POINT => "!"
  // FIXME:
    // LITERAL _ |DATA _| PROPERTY _|RANGE _|CHAR _
  | _ => assert false
  ;

type error =
  | Invalid_code_point
  | Eof
  | New_line;

let show_error =
  fun
  | Invalid_code_point => "Invalid code point"
  | Eof => "Unexpected end"
  | New_line => "New line";
