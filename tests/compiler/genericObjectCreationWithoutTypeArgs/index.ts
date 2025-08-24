// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericObjectCreationWithoutTypeArgs.ts`, Apache-2.0 License

class SS<T>{

}

var x1 = new SS<number>(); // OK
var x2 = new SS<number>;   // OK 
var x3 = new SS();         // OK
var x4 = new SS;           // OK
var x5: SS<unknown> = new SS();