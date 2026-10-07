// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/arrayEvery.ts`, Apache-2.0 License
var foo = ['aaa'];
var isString = (x) => (typeof x === 'string');
if (foo.every(isString)) {
  foo[0].slice(0);
}
