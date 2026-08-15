// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mismatchedExplicitTypeParameterAndArgumentType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function map<T, U>(xs: T[], f: (x: T) => U) {
    var ys: U[] = [];
    xs.forEach(x => ys.push(f(x)));
    return ys;
}

var r0 = map([1, ""], (x) => x.toString());
var r5 = map<any, any>([1, ""], (x) => x.toString());
var r6 = map<Object, Object>([1, ""], (x) => x.toString());
var r7 = map<number, string>([1, ""], (x) => x.toString()); // error
//~^ ERROR: Type 'string' is not assignable to type 'number'.
var r7b = map<number>([1, ""], (x) => x.toString()); // error
//~^ ERROR: Expected 2 type arguments, but got 1.
var r8 = map<any, string>([1, ""], (x) => x.toString());