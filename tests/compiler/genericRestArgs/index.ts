// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericRestArgs.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function makeArrayG<T>(...items: T[]): T[] { return items; }
var a1Ga = makeArrayG(1, ""); // no error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
var a1Gb = makeArrayG<any>(1, ""); 
var a1Gc = makeArrayG<Object>(1, ""); 
var a1Gd = makeArrayG<number>(1, ""); // error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.

function makeArrayGOpt<T>(item1?: T, item2?: T, item3?: T) {
    return [item1, item2, item3];
}
var a2Ga = makeArrayGOpt(1, ""); 
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
var a2Gb = makeArrayG<any>(1, "");
var a2Gc = makeArrayG<any[]>(1, ""); // error
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'any[]'.
