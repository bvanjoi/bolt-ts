// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgInference2WithError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Item {
    name: string;
}

declare function foo<T extends Item>(x?: T, y?: T): T;

var z7 = foo("abc", 5); // Error
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'undefined | Item'.
