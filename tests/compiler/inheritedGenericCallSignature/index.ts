// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritedGenericCallSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1<T> {

    (a: T): T;

}


interface Object {}

 

interface I2<T> extends I1<T[]> {

    b: T;

}

 

var x: I2<Date>;

 

var y = x(undefined);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Argument of type 'undefined' is not assignable to parameter of type 'Date[]'.

y.length;  // should not error
