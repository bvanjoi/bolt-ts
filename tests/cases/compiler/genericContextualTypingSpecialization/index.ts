// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericContextualTypingSpecialization.ts`, Apache-2.0 License

//@ run-fail

var b: number[];
b.reduce<number>((c, d) => c + d, 0); // should not error on '+'