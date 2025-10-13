// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportInterfaceClassAndValueWithDuplicatesInImportList.ts`, Apache-2.0 License

const foo = 1
class Foo {}
interface Foo {}

export {foo, Foo, Foo}
//~^ ERROR: Duplicate identifier 'Foo'.
