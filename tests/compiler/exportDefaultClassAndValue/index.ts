// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultClassAndValue.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015

const foo = 1
export default foo
//~^ ERROR: Cannot redeclare exported variable 'default'.
export default class Foo {}
//~^ ERROR: Cannot redeclare exported variable 'default'.
