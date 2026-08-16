// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeUsedAsValueError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Interface {

}

class SomeClass {

}

type TypeAliasForSomeClass = SomeClass;
type someType = { x: number };

function acceptsSomeType(a: someType) {

}

let one = Interface;
//~^ ERROR: Cannot find name 'Interface'.
let two = InterfaceNotFound;
//~^ ERROR: Cannot find name 'InterfaceNotFound'.
let three = TypeAliasForSomeClass;
//~^ ERROR: Cannot find name 'TypeAliasForSomeClass'.
let four = new TypeAliasForSomeClass();
//~^ ERROR: Cannot find name 'TypeAliasForSomeClass'.
let five = new TypeAliasForSomeClassNotFound();
//~^ ERROR: Cannot find name 'TypeAliasForSomeClassNotFound'.
let six = someType;
//~^ ERROR: Cannot find name 'someType'.
acceptsSomeType(someType);
//~^ ERROR: Cannot find name 'someType'.
acceptsSomeType(someTypeNotFound);
//~^ ERROR: Cannot find name 'someTypeNotFound'.
