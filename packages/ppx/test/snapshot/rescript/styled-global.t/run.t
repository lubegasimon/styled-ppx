  $ npx bsc -ppx "rewriter" -only-parse -bs-ast -bs-jsx 4 -bs-loc -bs-diagnose -bs-no-version-header -bs-ml-out -bs-no-builtin-ppx -bs-super-errors -color never -dsource input.res 2> output.ml
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
  

  $ cat output.ml
  ;;ignore
      (CssJs.global (({*j|html, body, #root, .class|*j})[@res.template ])
         [|(CssJs.margin `zero)|])

  $ npx rescript convert output.ml
  Error when converting output.ml
  File "", line 2, characters 22-23:
  Error: Syntax error: operator expected.
  
