// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/exportDefaultTypeAndClass.ts`, Apache-2.0 License

export default class Foo {}
type Bar = {}
export default Bar
//~^ ERROR: A module cannot have multiple default exports.
