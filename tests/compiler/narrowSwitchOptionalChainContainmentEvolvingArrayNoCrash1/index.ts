// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowSwitchOptionalChainContainmentEvolvingArrayNoCrash1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

let foo = [];
//~^ ERROR: Variable 'foo' implicitly has type 'any[]' in some locations where its type cannot be determined.

switch (foo?.length) {
  case 1:
    foo[0];
    //~^ ERROR: Variable 'foo' implicitly has an 'any[]' type.
}

let bar = [];

switch (bar?.length) {
  case 1: {
    bar.push("baz");
    const arr: string[] = bar;
  }
}