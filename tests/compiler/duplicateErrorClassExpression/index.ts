// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateErrorClassExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface ComplicatedTypeBase {
    [s: string]: ABase;
}
interface ComplicatedTypeDerived {
    [s: string]: ADerived;
}
interface ABase {
    a: string;
}
interface ADerived {
    b: string;
}
class Base {
    foo!: ComplicatedTypeBase;
}
const x = class Derived extends Base {
    foo!: ComplicatedTypeDerived;
    //~^ ERROR: 'ADerived' index signatures are incompatible.
    //~| ERROR: 'ADerived' index signatures are incompatible.
    //~| ERROR: 'ADerived' index signatures are incompatible.
    //~| ERROR: 'ADerived' index signatures are incompatible.
    //~| ERROR: 'ADerived' index signatures are incompatible.
}
let obj: { 3: string } = { 3: "three" };
obj[x];
//~^ ERROR: Type 'typeof Derived' cannot be used as an index type.