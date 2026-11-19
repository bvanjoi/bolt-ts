// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterCompatibilityAccrossDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

var a = {
   x: function <T>(y: T): T { return null; }
   //~^ ERROR: Type 'null' is not assignable to type 'T'.
}
var a2 = {
   x: function (y: any): any { return null; }
}
export interface I {
   x<T>(y: T): T;
}
export interface I2 {
   x(y: any): any;
}
 
var i: I;
var i2: I2;
 
a = i; // error
//~^ ERROR: Variable 'i' is used before being assigned.
i = a; // error
 
a2 = i2; // no error
//~^ ERROR: Variable 'i2' is used before being assigned.
i2 = a2; // no error
