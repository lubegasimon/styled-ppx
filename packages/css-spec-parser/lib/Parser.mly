%{

open Ast

%}

%token AMPERSAND
%token DOUBLE_AMPERSAND
%token DOUBLE_BAR
%token BAR
%token ASTERISK
%token PLUS
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_CURLY
%token RIGHT_CURLY
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token BAD_IDENT
%token BAD_URL
%token DOT
%token COMMA
%token SEMICOLON
// FIXME: Have a uniform format, with double score or not
%token DOUBLECOLON
%token COLON
%token QUESTION_MARK
%token IMPORTANT // FIXME: Is this not EXCLAMATION_POINT?
%token EXCLAMATION_POINT
%token CDO
%token CDC
%token WHITESPACE
// FIXME: This is bad naming of the token
%token PERCENTAGE_FROM_CSS_PARSER
%token EOF

%token <string> IDENT
%token <string> TAG
%token <string> OPERATOR
%token <string> COMBINATOR
%token <string> LITERAL
%token <string> CHAR
%token <string> DATA
%token <string> PROPERTY
%token <string> STRING
%token <string> BAD_STRING
%token <string> URL
%token <string> AT_KEYWORD
%token <string> AT_MEDIA
%token <string> AT_KEYFRAMES
%token <string> AT_RULE
%token <string> AT_RULE_STATEMENT
%token <string> DELIM
%token <string> FUNCTION
%token <string> NTH_FUNCTION
%token <string> UNICODE_RANGE // TODO: duplicate, but different types
%token <string> HASH_FROM_CSS_LEXER // FIXME: UGLY
%token <string> NUMBER_FROM_CSS_LEXER // FIXME: UGLY

%token <float> NUMBER
%token <float> PERCENTAGE

%token <float * string> DIMENSION
%token <string * string> FLOAT_DIMENSION
%token <string * string> DIMENSION_FROM_CSS_LEXER // FIXME: UGLY
%token <string list> INTERPOLATION
%token <string * [`ID | `UNRESTRICTED]> HASH
%token <[ `Comma | `Space ] * int * int option> RANGE

%start <value option> value_of_lex
%start <multiplier option> multiplier_of_lex
%start <stylesheet> stylesheet
%start <rule_list> declaration_list
%start <declaration> declaration
%start <rule_list> keyframes

%%
// This denotes the end of the grammar rules and the beginning of the code

let value_of_lex :=
  | EOF; { None }
  | v = value; EOF; { Some v }

let multiplier_of_lex :=
  | EOF; { None }
  | m = multiplier; EOF; { Some m }

let multiplier :=
  | ASTERISK; // *
    { Zero_or_more }
  | PLUS; // +
    { One_or_more }
  | QUESTION_MARK; // ?
    { Optional }
  | r = RANGE; // 1..4
    {
      match r with
      | (`Space, min, max) -> Repeat (min, max)
      | (`Comma, min, max) -> Repeat_by_comma (min, max)
    }
  | EXCLAMATION_POINT; // !
    { At_least_one }

let terminal ==
  | c = CHAR; { Delim c } // 'a'
  | l = LITERAL; { Keyword l } // "absolute"
  | d = DATA; { Data_type d } // <color>
  | p = PROPERTY; { Property_type p } // <'border'>

let terminal_multiplier(terminal) ==
  | t = terminal; { Terminal(t, One) } // "important"
  | t = terminal; m = multiplier; { Terminal(t, m) } // "important?"

let function_call :=
  | terminal_multiplier(terminal)
  | l = LITERAL; LEFT_PARENS; v = value; RIGHT_PARENS; { Function_call(l, v) } // rgb(1,2,3)

let group :=
  | function_call
  | LEFT_BRACKET; v = value; RIGHT_BRACKET; { v } // [ v ]
  | LEFT_BRACKET; v = value; RIGHT_BRACKET; m = multiplier; { Group(v, m) } // [ v ]!

let combinator(sep, sub, kind) ==
  | vs = separated_nonempty_list(sep, sub); ~ = kind;
    { match vs with | v::[] -> v | vs -> Combinator(kind, vs) }

let static_expr ==
  | combinator(| {}, group, | { Static }) // A B
let and_expr ==
  | combinator(DOUBLE_AMPERSAND, static_expr, | { And }) // A && B
let or_expr ==
  | combinator(DOUBLE_BAR, and_expr, | { Or }) // A || B
let xor_expr ==
  | combinator(BAR, or_expr, | { Xor }) // A | B

let value :=
  | xor_expr
  (* TODO: this is clearly a workaround *)
  | terminal_multiplier(| RIGHT_PARENS; { Keyword ")" } | LEFT_PARENS; { Keyword "(" })

/*
  production rule
*/
stylesheet: s = stylesheet_without_eof; EOF { s }
/*
  [stylesheet] represents the name of the production rule.

  [s = stylesheet_without_eof] indicates the action to take when [stylesheet] is parsed.
  it assigns the result of parsing [stylesheet_without_eof] to variable [s].

  [stylesheet_without_eof] is the production rule or a set of rules that define the syntax
  of the stylesheet but without considering the end of file

  [EOF { s }] -> indicates what should happen when the parser reaches EOF, it returns the
  value of [s] which represents the parsed [stylesheet].
*/

stylesheet_without_eof: rs = loc(list(rule)) { rs }

declaration_list:
  | WHITESPACE? EOF { ([], Lex_buffer.make_loc $startpos $endpos) }
  | ds = loc(declarations) EOF { ds }

/* keyframe may contain {} */
keyframe: rules = nonempty_list(keyframe_style_rule) { rules }

keyframes:
  | rules = loc(keyframe) EOF { rules }
  | rules = brace_block(loc(keyframe)) EOF { rules }

/* Adds location as a tuple */
loc(X): x = X {
  (x, Lex_buffer.make_loc $startpos(x) $endpos(x))
}

/* Handle skipping whitespace */
skip_ws (X): x = delimited(WHITESPACE?, X, WHITESPACE?) { x }
skip_ws_right (X): x = X; WHITESPACE? { x }
// skip_ws_left (X): WHITESPACE? x = X; { x }

/* TODO: Remove empty_brace_block */
/* {} */
empty_brace_block: pair(LEFT_BRACKET, RIGHT_BRACKET) { [] }

/* TODO: Remove SEMICOLON? from brace_block(X) */
/* { ... } */
brace_block(X): xs = delimited(LEFT_BRACKET, X, RIGHT_BRACKET) SEMICOLON? { xs }

/* [] */
bracket_block (X): xs = delimited(LEFT_BRACKET, X, RIGHT_BRACKET) { xs }

/* () */
paren_block (X): xs = delimited(LEFT_PARENS, X, RIGHT_PARENS) { xs }

/* https://www.w3.org/TR/mediaqueries-5 */
/* Parsing with this approach is almost as good the entire spec */

/*
  In this context, prelude refers to the initial section of the code or
  declarations that come before the main body of the parsing rules.
  The prelude contatins esential definitions, imports, or set up code
  necessary to support the parsing process. It is often used to set up
  environment for parsing and may include the following;
    - import statements: import modules or libraries that provide parsing-related
    functions and utilities
    - Global declarations: for variables, data structures, or functions that are
    used throughout the parsing process
    - lexer Setup :  If the lexer is separate from the parser, the prelude may
    contain configuration or setup code for the lexer.
    - parser configuration: It may include settings or configuration options for
    the parser itself, such as error handling behavior or parsing modes
    - custom data types
    - utility functions: utility functions or helper functions that assist in the
    parsing process.
    - Documentation: explain the purpose and usage of the parser.
*/
prelude: xs = loption(nonempty_list(loc(value_in_prelude))) { xs }

/* Missing grammars: */
/* (width >= 600px) */
/* (400px < width < 1000px) */
/* (not (color)) and (not (hover)) */
/* Combinator_ "," */
media_query_prelude_item:
  | i = IDENT { Ident i }
  | v = INTERPOLATION { Variable v }
  | xs = paren_block(prelude) { Paren_block xs }

media_query_prelude: q = nonempty_list(loc(skip_ws(media_query_prelude_item))) { q }

/* https://www.w3.org/TR/css-syntax-3/#at-rules */
at_rule:
  /* @media (min-width: 16rem) { ... } */
  | name = loc(AT_MEDIA) WHITESPACE?
    prelude = loc(media_query_prelude) WHITESPACE?
    ds = brace_block(loc(declarations)) WHITESPACE? {
    { name = name;
      prelude;
      block = Rule_list ds;
      loc = Lex_buffer.make_loc $startpos $endpos;
    }
  }
  /* @media (min-width: 16rem) {} */
  | name = loc(AT_MEDIA) WHITESPACE?
    prelude = loc(media_query_prelude) WHITESPACE?
    b = loc(empty_brace_block) WHITESPACE? {
    { name = name;
      prelude;
      block = Rule_list b;
      loc = Lex_buffer.make_loc $startpos $endpos;
    }
  }
  /* @keyframes animationName { ... } */
//   | name = loc(AT_KEYFRAMES) WHITESPACE?
//     i = IDENT WHITESPACE?
//     block = brace_block(keyframe) {
//     let item = (Ident i, Lex_buffer.make_loc $startpos(i) $endpos(i)) in
//     let prelude = ([item], Lex_buffer.make_loc $startpos $endpos) in
//     let block = Rule_list (block, Lex_buffer.make_loc $startpos $endpos) in
//     { name = name;
//       prelude;
//       block;
//       loc = Lex_buffer.make_loc $startpos $endpos;
//     }
//   }
//   /* @keyframes animationName {} */
//   | name = loc(AT_KEYFRAMES) WHITESPACE?
//     i = IDENT WHITESPACE?
//     s = loc(empty_brace_block) {
//     let item = ((Ident i), Lex_buffer.make_loc $startpos(i) $endpos(i)) in
//     let prelude = ([item], Lex_buffer.make_loc $startpos $endpos) in
//     let empty_block = Rule_list s in
//     ({ name = name;
//       prelude = prelude;
//       block = empty_block;
//       loc = Lex_buffer.make_loc $startpos $endpos;
//     }): at_rule
//   }
//   /* @charset */
//   | name = loc(AT_RULE_STATEMENT) WHITESPACE?
//     xs = loc(prelude) WHITESPACE? SEMICOLON? {
//     { name = name;
//       prelude = xs;
//       block = Empty;
//       loc = Lex_buffer.make_loc $startpos $endpos;
//     }
//   }
//   /* @support { ... } */
//   /* @page { ... } */
//   /* @{{rule}} { ... } */
//   | name = loc(AT_RULE) WHITESPACE?
//     xs = loc(prelude) WHITESPACE?
//     s = brace_block(stylesheet_without_eof) WHITESPACE? {
//     { name = name;
//       prelude = xs;
//       block = Stylesheet s;
//       loc = Lex_buffer.make_loc $startpos $endpos;
//     }
//   }

percentage: n = NUMBER PERCENTAGE { n }

/* keyframe allows stylesheet by defintion, but we restrict the usage to: */
keyframe_style_rule:
  /* from {} to {} */
  | WHITESPACE? id = IDENT WHITESPACE?
    declarations = brace_block(loc(declarations)) WHITESPACE? {
    let prelude = [(SimpleSelector (Type id), Lex_buffer.make_loc $startpos(id) $endpos(id))] in
    Style_rule {
      prelude = (prelude, Lex_buffer.make_loc $startpos(id) $endpos(id));
      loc = Lex_buffer.make_loc $startpos $endpos;
      block = declarations;
    }
  }
  /* TODO: Support percentage in simple_selector and have selector parsing here */
  | WHITESPACE? p = percentage; WHITESPACE?
    declarations = brace_block(loc(declarations)) WHITESPACE? {
    let item = Percentage p in
    let prelude = [(SimpleSelector item, Lex_buffer.make_loc $startpos(p) $endpos(p))] in
    Style_rule {
      prelude = (prelude, Lex_buffer.make_loc $startpos(p) $endpos(p));
      loc = Lex_buffer.make_loc $startpos $endpos;
      block = declarations;
    }
  }
  | percentages = separated_list(COMMA, skip_ws(percentage));
    declarations = brace_block(loc(declarations)) WHITESPACE? {
    let prelude = percentages
      |> List.map (fun percent -> Percentage percent)
      |> List.map (fun p ->
        (SimpleSelector p, Lex_buffer.make_loc $startpos(percentages) $endpos(percentages))
      ) in
    Style_rule {
      prelude = (prelude, Lex_buffer.make_loc $startpos(percentages) $endpos(percentages));
      loc = Lex_buffer.make_loc $startpos $endpos;
      block = declarations;
    }
  }
  /* TODO: Handle separated_list(COMMA, percentage) */
; // FIXME: Is this not weird?

selector_list:
  | selector = loc(selector) WHITESPACE? { [selector] }
  | selector = loc(selector) WHITESPACE? COMMA WHITESPACE? seq = selector_list WHITESPACE? { selector :: seq }

/* .class {} */
style_rule:
  | prelude = loc(selector_list) WHITESPACE?
    block = loc(empty_brace_block) {
    { prelude;
      block;
      loc = Lex_buffer.make_loc $startpos $endpos;
    }
  }
  | prelude = loc(selector_list) WHITESPACE?
    declarations = brace_block(loc(declarations)) {
    { prelude;
      block = declarations;
      loc = Lex_buffer.make_loc $startpos $endpos;
    }
  }

values: xs = nonempty_list(loc(skip_ws(value))) { xs }

declarations:
  | WHITESPACE? xs = nonempty_list(rule) SEMICOLON? { xs }
  | WHITESPACE? xs = separated_nonempty_list(SEMICOLON, rule) SEMICOLON? { xs }

%inline rule:
  /* Rule can have declarations, since we have nesting, so both style_rules and
  declarations can live side by side. */
  | d = skip_ws_right(declaration_without_eof); { Declaration d }
  | r = skip_ws(at_rule) { At_rule r }
  | s = skip_ws(style_rule) { Style_rule s }

declaration: d = declaration_without_eof; EOF { d }

declaration_without_eof:
  /* property: value; */
  | WHITESPACE? property = loc(IDENT)
    WHITESPACE? COLON
    WHITESPACE? value = loc(values)
    WHITESPACE? important = loc(boption(IMPORTANT))
    WHITESPACE? SEMICOLON?
    WHITESPACE? {
    { name = property;
      value;
      important;
      loc = Lex_buffer.make_loc $startpos $endpos;
    }
  }

nth_payload:
  /* TODO implement [of <complex-selector-list>]? */
  /* | complex = complex_selector_list; { NthSelector complex } */
  /* <An+B> */
  /* 2 */
  | a = NUMBER { Nth (A (int_of_string a)) }
  /* 2n */
  | a = DIMENSION { Nth (AN (int_of_string (fst a))) }
  /* 2n-1 */
  | a = DIMENSION WHITESPACE? combinator = COMBINATOR b = NUMBER {
    let b = int_of_string b in
    Nth (ANB (((int_of_string (fst a)), combinator, b)))
  }
  /* This is a hackish solution where combinator isn't cached because the lexer
  assignes the `-` to NUMBER. This could be solved by leftassoc */
  | a = DIMENSION WHITESPACE? b = NUMBER {
    let b = Int.abs (int_of_string (b)) in
    Nth (ANB (((int_of_string (fst a)), "-", b)))
  }
  | n = IDENT WHITESPACE? {
    match n with
      | "even" -> Nth (Even)
      | "odd" -> Nth (Odd)
      | "n" -> Nth (AN 1)
      | _ -> (
        let first_char = String.get n 0 in
        let a = if first_char = '-' then -1 else 1 in
        Nth (AN a)
      )
  }
  /* n-1 */
  /* n */
  /* -n */
  | n = IDENT WHITESPACE? combinator = COMBINATOR b = NUMBER {
    let first_char = String.get n 0 in
    let a = if first_char = '-' then -1 else 1 in
    Nth (ANB ((a, combinator, int_of_string b)))
  }
  /* TODO: Support "An+B of Selector" */

/* <pseudo-class-selector> = ':' <ident-token> | ':' <function-token> <any-value> ')' */
pseudo_class_selector:
  | COLON i = IDENT { (Pseudoclass(PseudoIdent i)) } /* :visited */
  | COLON f = FUNCTION xs = loc(selector) RIGHT_PARENS /* :not() */ {
    (Pseudoclass(Function({ name = f; payload = xs })))
  }
  | COLON f = NTH_FUNCTION xs = loc(nth_payload) RIGHT_PARENS /* :nth() */ {
    (Pseudoclass(NthFunction({ name = f; payload = xs })))
  }
  /* TODO: <function-token> and <any-value> */
;

/* "~=" | "|=" | "^=" | "$=" | "*=" | "=" */
attr_matcher: o = OPERATOR { o }

/* <attribute-selector> = '[' <wq-name> ']' | '[' <wq-name> <attr-matcher> [  <string-token> | <ident-token> ] <attr-modifier>? ']' */
attribute_selector:
  /* https://www.w3.org/TR/selectors-4/#type-nmsp */
  /* We don't support namespaces in wq-name (`ns-prefix?`). We treat it like a IDENT */
  /* [ <wq-name> ] */
  | LEFT_BRACKET; WHITESPACE?
    i = IDENT WHITESPACE?
    RIGHT_BRACKET {
    Attribute(Attr_value i)
  }
  /* [ wq-name = "value"] */
  | LEFT_BRACKET; WHITESPACE?
    i = IDENT WHITESPACE?
    m = attr_matcher; WHITESPACE?
    v = STRING; WHITESPACE?
    RIGHT_BRACKET {
    Attribute(
      To_equal({
        name = i;
        kind = m;
        value = Attr_string v
      })
    )
  }
  /* [ wq-name = value] */
  | LEFT_BRACKET; WHITESPACE?
    i = IDENT WHITESPACE?
    m = attr_matcher; WHITESPACE?
    v = IDENT WHITESPACE?
    RIGHT_BRACKET {
    Attribute(
      To_equal({
        name = i;
        kind = m;
        value = Attr_ident v
      })
    )
  }
  /* TODO: add attr-modifier */
;

/* <id-selector> = <hash-token> */
id_selector: h = HASH { Id h }

/* <class-selector> = '.' <ident-token> */
class_selector:
  | DOT id = IDENT { Class id }
  /* TODO: Fix this: Here we need to add TAG in case some ident is an actual tag :( */
  | DOT tag = TAG { Class tag }

/* <subclass-selector> = <id-selector> | <class-selector> | <attribute-selector> | <pseudo-class-selector> */
subclass_selector:
  | id = id_selector { id } /* #id */
  | c = class_selector { c } /* .class */
  | a = attribute_selector { a } /* [attr] */
  | pcs = pseudo_class_selector { Pseudo_class pcs } /* :pseudo-class */
  | DOT v = INTERPOLATION { ClassVariable v } /* .$(Variable) as subclass_selector */

selector:
  /* By definition a selector can be one of those kinds, since inside
  complex_selector we arrive to simple and compound, we discard those branches here.
  The reason is because are in a LR(1) parser which makes those structures hard
  to accomplish by following the CSS spec.

  Check <non_complex_selector>
  */
  /* | xs = skip_ws_right(simple_selector) { SimpleSelector xs } */
  /* | xs = skip_ws_right(compound_selector) { CompoundSelector xs } */
  | xs = skip_ws_right(complex_selector) { ComplexSelector xs }

type_selector:
  | AMPERSAND; { Ampersand } /* & {} https://drafts.csswg.org/css-nesting/#nest-selector */
  | ASTERISK; { Universal } /* * {} */
  | v = INTERPOLATION { Variable v } /* $(Module.value) {} */
  /* TODO: type_selector should work with IDENTs, but there's a bunch of grammar
    conflicts with IDENT on value and others, we replaced with TAG, a
    list of valid HTML tags that does the job done, but this should be fixed. */
  | type_ = IDENT; { Type type_ } /* a {} */
  | type_ = TAG; { Type type_ } /* a {} */

/* <simple-selector> = <type-selector> | <subclass-selector> */
/* <simple-selector> = <self-selector> | <type-selector> | <subclass-selector> */
simple_selector:
  | t = type_selector { t }
  /* With <coumpound-selector> that subclass_selector becomes irrelevant */
  /* | sb = subclass_selector { Subclass sb } */ /* #a, .a, a:visited, a[] */

pseudo_element_selector:
  DOUBLECOLON; pse = IDENT { Pseudoelement pse } /* ::after */

pseudo_list:
  /* ::after:hover */
  /* ::after:hover:visited */
  | element = pseudo_element_selector class_list = list(pseudo_class_selector) {
    element :: class_list
  }

// /* <compound-selector> = [
//     <type-selector>? <subclass-selector>*
//     [ <pseudo-element-selector> <pseudo-class-selector>* ]*
//   ]!

//   compound_selector expects first to be a <type-selector>, since we support
//   nesting and that can be a few more things look at <simple-selector> */
compound_selector:
  /* #id::hover:visited */
  | sub = nonempty_list(subclass_selector) ps = pseudo_list {
     {
      type_selector = None;
      subclass_selectors = sub;
      pseudo_selectors = ps;
    }
  }
  /* a#id */
  | t = type_selector sub = nonempty_list(subclass_selector) {
     {
      type_selector = Some t;
      subclass_selectors = sub;
      pseudo_selectors = [];
    }
  }
  /* #hover */
  | sub = nonempty_list(subclass_selector) {
     {
      type_selector = None;
      subclass_selectors = sub;
      pseudo_selectors = [];
    }
  }
  /* a::after:hover */
  | t = type_selector ps = pseudo_list {
     {
      type_selector = Some t;
      subclass_selectors = [];
      pseudo_selectors = ps;
    }
  }
  /* ::after:hover */
  | ps = pseudo_list {
     {
      type_selector = None;
      subclass_selectors = [];
      pseudo_selectors = ps;
    }
  }

combinator_sequence:
  | WHITESPACE s = non_complex_selector { (None, s) }
  | s = non_complex_selector WHITESPACE? { (None, s) }
  | c = COMBINATOR WHITESPACE? s = non_complex_selector WHITESPACE? { (Some c, s) }

%inline non_complex_selector:
  | s = simple_selector { SimpleSelector s }
  | s = compound_selector { CompoundSelector s }

/* <complex-selector> = <compound-selector> [ <combinator>? <compound-selector> ]* */
complex_selector:
  | left = skip_ws_right(non_complex_selector) { Selector left }
  | left = non_complex_selector WHITESPACE? seq = nonempty_list(combinator_sequence) {
    Combinator_ {
      left = left;
      right = seq;
    }
  }

/* value_in_prelude we transform WHITESPACE_* into Delim with white spaces inside
in value we transform to regular Delim
The rest of value_in_prelude and value should be sync */
value_in_prelude:
  | b = paren_block(prelude) { Paren_block b }
  | b = bracket_block(prelude) { Bracket_block b }
  | n = percentage { Percentage n }
  | i = IDENT { Ident i }
  | i = TAG { Ident i }
  | s = STRING { String s }
  | c = COMBINATOR { Combinator_ c}
  | o = OPERATOR { Operator o }
  | d = DELIM { Delim d }
  | DOT { Delim "." }
  | COLON { Delim ":" }
  | DOUBLECOLON { Delim "::" }
  | h = HASH_FROM_CSS_LEXER { Hash h }
  | COMMA { Delim "," }
  | n = NUMBER_FROM_CSS_LEXER { Number n }
  | r = UNICODE_RANGE { Unicode_range r }
  | d = FLOAT_DIMENSION { Float_dimension d }
  | d = DIMENSION_FROM_CSS_LEXER { Dimension d }
  | v = INTERPOLATION { Variable v } /* $(Lola.value) */
  | f = loc(FUNCTION) xs = loc(prelude) RIGHT_PARENS; { Function (f, xs) } /* calc() */
  | u = URL { Uri u } /* url() */
  | WHITESPACE { Delim " " }

// value:
//   | b = paren_block(values) { Paren_block b }
//   | b = bracket_block(values) { Bracket_block b }
//   | n = percentage { Percentage n }
//   | i = IDENT { Ident i }
//   | i = TAG { Ident i }
//   | s = STRING { String s }
//   | c = COMBINATOR { Combinator_ c}
//   | o = OPERATOR { Operator o }
//   | d = DELIM { Delim d }
//   | DOT { Delim "." }
//   | ASTERISK { Delim "*" }
//   | COLON { Delim ":" }
//   | h = HASH_FROM_CSS_LEXER { Hash h }
//   | DOUBLECOLON { Delim "::" }
//   | COMMA { Delim "," }
//   | n = NUMBER_FROM_CSS_LEXER { Number n }
//   | r = UNICODE_RANGE { Unicode_range r }
//   | d = FLOAT_DIMENSION { Float_dimension d }
//   | d = DIMENSION_FROM_CSS_LEXER { Dimension d }
//   | v = INTERPOLATION { Interpolation v } /* $(Lola.value) */
//   | f = loc(FUNCTION) v = loc(values) RIGHT_PARENS; { Function (f, v) } /* calc() */
//   | u = URL { Uri u } /* url() */
