// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionTypesLackingReturnTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// Error (no '=>')
function f(x: ()) {
  //~^ ERROR: Expected '=>'.
  //~| ERROR: Identifier expected.
}

// Error (no '=>')
var g: (param);
//~^ ERROR: Cannot find name 'param'.

// Okay
var h: { () }

// Okay
var i: { new () }