// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInArrayElementLibEs2015.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[es2015]

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
