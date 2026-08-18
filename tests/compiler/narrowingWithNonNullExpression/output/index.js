// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingWithNonNullExpression.ts`, Apache-2.0 License
var m = ''.match('');
m && m[0];
m[0] && m[0];