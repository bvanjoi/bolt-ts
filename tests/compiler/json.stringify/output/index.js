// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/json.stringify.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var value = null;
JSON.stringify(value, undefined, 2);
JSON.stringify(value, null, 2);
JSON.stringify(value, ['a', 1], 2);
JSON.stringify(value, (k) => (undefined), 2);
JSON.stringify(value, undefined, 2);