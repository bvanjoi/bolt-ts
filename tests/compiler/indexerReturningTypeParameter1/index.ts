// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexerReturningTypeParameter1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface f {
    groupBy<T>(): { [key: string]: T[]; };
}
var a: f;
var r = a.groupBy();
//~^ ERROR: Variable 'a' is used before being assigned.
//~| ERROR: Variable 'a' is used before being assigned.

class c {
    groupBy<T>(): { [key: string]: T[]; } {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type '{ [key: string]: T[] }'.
    }
}
var a2: c;
var r2 = a2.groupBy();
//~^ ERROR: Variable 'a2' is used before being assigned.
//~| ERROR: Variable 'a2' is used before being assigned.
