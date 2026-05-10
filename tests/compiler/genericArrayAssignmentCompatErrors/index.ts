// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericArrayAssignmentCompatErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var myCars=new Array(); 
var myCars2 = new [];     //~ERROR: This expression is not constructable.
var myCars3 = new Array({});
declare var myCars4: Array; // error
//~^ ERROR: Generic type 'Array<T>' requires 1 type argument.
declare var myCars5: Array<any>[];
 
myCars = myCars2;
myCars = myCars3;
myCars = myCars4;
myCars = myCars5;
 
myCars2 = myCars;
myCars2 = myCars3;
myCars2 = myCars4;
myCars2 = myCars5;
 
myCars3 = myCars;
myCars3 = myCars2;
myCars3 = myCars4;
myCars3 = myCars5;   
