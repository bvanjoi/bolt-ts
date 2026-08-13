// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typecheckCommaExpression.ts`, Apache-2.0 License

(a, b)
//~^ ERROR: Cannot find name 'b'.
//~| ERROR: Cannot find name 'a'.
//~| ERROR: Left side of comma operator is unused and has no side effects.