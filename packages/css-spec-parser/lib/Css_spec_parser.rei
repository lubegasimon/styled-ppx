include (module type of {
  include Ast;
});

module Location = Ppxlib.Location;

module Tokens = Tokens

type token_with_location = {
  txt: result(Tokens.token, (Tokens.token, Tokens.error)),
  loc: Location.t,
};

let string_of_value: Ast.value => string;
let value_of_string: string => option(Ast.value);
let from_string : string => result(list(token_with_location), [> `Frozen ]) 
