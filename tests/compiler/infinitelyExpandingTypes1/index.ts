// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/infinitelyExpandingTypes1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface List<T> {
    data: T;
    next: List<T>;
    owner: List<List<T>>;
}


interface MyList<T> {
    data: T;
    next: MyList<T>;
    owner: MyList<MyList<T>>;
}

var l: List<number>;
var m: MyList<number>;

l == m; // should error
//~^ ERROR: Variable 'l' is used before being assigned.
//~| ERROR: Variable 'm' is used before being assigned.

var l2: List<string>;

l == l2; // should error;
//~^ ERROR: Variable 'l' is used before being assigned.
//~| ERROR: Variable 'l2' is used before being assigned.
//~| ERROR: This comparison appears to be unintentional because the types 'List<number>' and 'List<string>' have no overlap.

l == l; // should not error
//~^ ERROR: Variable 'l' is used before being assigned.
//~| ERROR: Variable 'l' is used before being assigned.