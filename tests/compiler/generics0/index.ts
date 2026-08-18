// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics0.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

interface G<T> {
    x: T;
}

var v2: G<string>;

var z = v2.x; // 'y' should be of type 'string'
//~^ ERROR: Variable 'v2' is used before being assigned.