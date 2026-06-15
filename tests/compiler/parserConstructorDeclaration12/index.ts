// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parserConstructorDeclaration12.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
  constructor<>() { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor<> () { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor <>() { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor <> () { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor< >() { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor< > () { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor < >() { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
  constructor < > () { }
  //~^ ERROR: Type parameter list cannot be empty.
  //~| ERROR: Multiple constructor implementations are not allowed.
}

class D {
  constructor<T>() { }
  //~^ ERROR: Type parameters cannot appear on a constructor declaration.
}