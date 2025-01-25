// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/arrayLiteralAndArrayConstructorEquivalence1.ts`, Apache-2.0 License

var myCars=new Array(); 
var myCars3 = new Array({});
var myCars4: Array; // error
//~^ ERROR: Generic type 'Array' requires 1 type argument.
var myCars5: Array<any>[];
 
myCars = myCars3;
myCars = myCars4;
myCars = myCars5;
 
myCars3 = myCars;
myCars3 = myCars4;
myCars3 = myCars5;   
