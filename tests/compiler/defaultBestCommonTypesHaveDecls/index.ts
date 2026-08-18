// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultBestCommonTypesHaveDecls.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var obj1: {};
obj1.length;
//~^ ERROR: Property 'length' does not exist on type '{ }'.

var obj2: Object;
obj2.length;
//~^ ERROR: Property 'length' does not exist on type 'Object'.

function concat<T>(x: T, y: T): T { return null; }
var result = concat(1, ""); // error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type '1'.
var elementCount = result.length; 
//~^ ERROR: Property 'length' does not exist on type 'number'.

function concat2<T, U>(x: T, y: U) { return null; }
var result2 = concat2(1, ""); // result2 will be number|string
var elementCount2 = result.length; 
//~^ ERROR: Property 'length' does not exist on type 'number'.

