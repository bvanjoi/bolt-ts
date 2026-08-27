// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nativeToBoxedTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var N = new Number();
var n = 100;
n = N;
//~^ ERROR: Type 'Number' is not assignable to type 'number'.

var S = new String();
var s = "foge";
s = S;
//~^ ERROR: Type 'String' is not assignable to type 'string'.

var B = new Boolean();
var b = true;
b = B;
//~^ ERROR: Type 'Boolean' is not assignable to type 'boolean'.

var sym: symbol; 
var Sym: Symbol;
sym = Sym;
//~^ ERROR: Type 'Symbol' is not assignable to type 'symbol'.
//~| ERROR: Variable 'Sym' is used before being assigned.
