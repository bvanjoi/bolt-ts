// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInArrayElementLibEs5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[es5]

// See: https://github.com/microsoft/TypeScript/pull/53280#discussion_r1138684984

declare function test(
  arg: Record<string, (arg: string) => void> | Array<(arg: number) => void>
): void;

test([
  (arg) => {
    //~^ ERROR: Parameter 'arg' implicitly has an 'any' type.
    arg; // number
  },
]);

test({
  a: (arg) => {
    const a: number = arg;
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
  }
});
