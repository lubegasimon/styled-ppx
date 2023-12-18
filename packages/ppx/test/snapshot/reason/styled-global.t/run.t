  $ refmt --parse re --print ml input.re > output.ml
  $ standalone --impl output.ml -o output.ml
  Selector: (ComplexSelector ((Selector (SimpleSelector ((Type "html"), 0))), 1))
  
  Selector: (SimpleSelector ((Type "html"), 0))
  
  Selector: (ComplexSelector ((Selector (SimpleSelector ((Type "body"), 1))), 2))
  
  Selector: (SimpleSelector ((Type "body"), 1))
  
  Selector: (ComplexSelector (
     (Selector
        (CompoundSelector (
           { type_selector = None; subclass_selectors = [(Id "root")];
             pseudo_selectors = [] },
           2))),
     3))
  
  Selector: (CompoundSelector (
     { type_selector = None; subclass_selectors = [(Id "root")];
       pseudo_selectors = [] },
     2))
  
  Selector: (ComplexSelector (
     (Selector
        (CompoundSelector (
           { type_selector = None; subclass_selectors = [(Class "class")];
             pseudo_selectors = [] },
           3))),
     4))
  
  Selector: (CompoundSelector (
     { type_selector = None; subclass_selectors = [(Class "class")];
       pseudo_selectors = [] },
     3))
  
  $ refmt --parse ml --print re output.ml
  ignore(
    CssJs.global({js|html, body, #root, .class|js}, [|CssJs.margin(`zero)|]),
  );
