// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingWithNonNullExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const m = ''.match('');
m! && m[0];
m?.[0]! && m[0];
