  $ npx bsc -ppx "rewriter" -only-parse -bs-ast -bs-jsx 4 -bs-loc -bs-diagnose -bs-no-version-header -bs-ml-out -bs-no-builtin-ppx -bs-super-errors -color never -dsource input.res 2> output.ml
  Selector: (ComplexSelector (
     (Selector
        (CompoundSelector (
           { type_selector = (Some Ampersand);
             subclass_selectors =
             [(Pseudo_class (Pseudoclass (PseudoIdent "hover")))];
             pseudo_selectors = [] },
           0))),
     1))
  
  Selector: (CompoundSelector (
     { type_selector = (Some Ampersand);
       subclass_selectors =
       [(Pseudo_class (Pseudoclass (PseudoIdent "hover")))];
       pseudo_selectors = [] },
     0))
  
  Selector: (ComplexSelector (
     Combinator {left = (SimpleSelector (Ampersand, 1));
       right = [((Some ">"), (SimpleSelector ((Type "p"), 1)))]},
     2))
  
  Selector: (SimpleSelector (Ampersand, 1))
  
  Selector: (SimpleSelector ((Type "p"), 1))
  

No clue why bsc generates a invalid syntax, but it does. This removes this particual bit.
  $ sed -e 's/.I1//g' output.ml > fixed.ml

  $ npx rescript convert fixed.ml
  Error when converting fixed.ml
  File "", line 494, characters 70-75:
  Error: Invalid literal 600px
  







