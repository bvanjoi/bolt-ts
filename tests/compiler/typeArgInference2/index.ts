// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgInference2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Item {
    name: string;
}

declare function foo<T extends Item>(x?: T, y?: T): T;

var z1 = foo(null);                   // any
//~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'undefined | Item'.
var z2 = foo();                       // Item
var z3 = foo({ name: null });         // { name: any }
//~^ ERROR: Type 'null' is not assignable to type 'string'.
var z4 = foo({ name: "abc" });        // { name: string }
var z5 = foo({ name: "abc", a: 5 });  // { name: string; a: number }
var z6 = foo({ name: "abc", a: 5 }, { name: "def", b: 5 });  // error
