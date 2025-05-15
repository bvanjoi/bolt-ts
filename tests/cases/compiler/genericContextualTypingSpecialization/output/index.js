// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericContextualTypingSpecialization.ts`, Apache-2.0 License
//@ run-fail
var b;
b.reduce((c, d) => c + d, 0);