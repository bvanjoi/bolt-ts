// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionType.ts`, Apache-2.0 License
function salt() {}
salt.apply('hello', []);
(new Function('return 5'))();