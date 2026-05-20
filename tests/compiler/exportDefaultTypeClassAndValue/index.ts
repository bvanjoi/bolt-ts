// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultTypeClassAndValue.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

const foo = 1
export default foo
//~^ ERROR: Cannot redeclare exported variable 'default'.
export default class Foo {}
//~^ ERROR: Cannot redeclare exported variable 'default'.
type Bar = {}
export default Bar
//~^ ERROR: A module cannot have multiple default exports.
