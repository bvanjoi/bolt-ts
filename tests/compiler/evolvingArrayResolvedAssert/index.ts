// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/evolvingArrayResolvedAssert.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

var C = [];
//~^ ERROR: Variable 'C' implicitly has type 'any[]' in some locations where its type cannot be determined.
//~| ERROR: Variable 'C' implicitly has type 'any[]' in some locations where its type cannot be determined.
//~| ERROR: Variable 'C' implicitly has type 'any[]' in some locations where its type cannot be determined.
//~| ERROR: Variable 'C' implicitly has type 'any[]' in some locations where its type cannot be determined.
for (var a in C) {
  //~^ ERROR: Variable 'C' implicitly has an 'any[]' type.
  //~| ERROR: Variable 'C' implicitly has an 'any[]' type.
    if (C.hasOwnProperty(a)) {
  //~^ ERROR: Variable 'C' implicitly has an 'any[]' type.
  //~| ERROR: Variable 'C' implicitly has an 'any[]' type.
    }
}