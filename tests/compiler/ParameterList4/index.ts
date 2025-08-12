// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ParameterList4.ts`, Apache-2.0 License

function F(public A) {
//~^ ERROR: A parameter property is only allowed in a constructor implementation
}

interface I {
  f(public A);
  //~^ ERROR: A parameter property is only allowed in a constructor implementation
  new (public A)
  //~^ ERROR: A parameter property is only allowed in a constructor implementation
  (public A);
  //~^ ERROR: A parameter property is only allowed in a constructor implementation
}

let o = {
  f(public a) {}
//~^ ERROR: A parameter property is only allowed in a constructor implementation
}

let f = function(public A) {}
//~^ ERROR: A parameter property is only allowed in a constructor implementation
