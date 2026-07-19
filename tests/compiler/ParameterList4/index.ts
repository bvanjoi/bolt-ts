// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ParameterList4.ts`, Apache-2.0 License

function F(public A) {
//~^ ERROR: A parameter property is only allowed in a constructor implementation
//~| ERROR: Parameter 'A' implicitly has an 'any' type.
}

interface I {
  f(public A);
  //~^ ERROR: A parameter property is only allowed in a constructor implementation
  //~| ERROR: Parameter 'A' implicitly has an 'any' type.
  //~| ERROR: 'f', which lacks return-type annotation, implicitly has an 'any' return type.
  new (public A)
  //~^ ERROR: A parameter property is only allowed in a constructor implementation
  //~| ERROR: Parameter 'A' implicitly has an 'any' type.
  //~| ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
  (public A);
  //~^ ERROR: A parameter property is only allowed in a constructor implementation
  //~| ERROR: Parameter 'A' implicitly has an 'any' type.
  //~| ERROR: Call signature, which lacks return-type annotation, implicitly has an 'any' return type.
}

let o = {
  f(public a) {}
//~^ ERROR: A parameter property is only allowed in a constructor implementation
  //~| ERROR: Parameter 'a' implicitly has an 'any' type.
}

let f = function(public A) {}
//~^ ERROR: A parameter property is only allowed in a constructor implementation
//~| ERROR: Parameter 'A' implicitly has an 'any' type.
