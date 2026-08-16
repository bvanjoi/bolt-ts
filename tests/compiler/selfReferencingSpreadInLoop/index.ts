// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/selfReferencingSpreadInLoop.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

let additional = [];
//~^ ERROR: Variable 'additional' implicitly has type 'any[]' in some locations where its type cannot be determined.
for (const subcomponent of [1, 2, 3]) {
    additional = [...additional, subcomponent];
  //~^ ERROR: Variable 'additional' implicitly has an 'any[]' type.
}
