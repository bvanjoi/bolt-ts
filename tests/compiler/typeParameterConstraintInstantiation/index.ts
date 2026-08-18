// From `github.com/microsoft/TypeScript/blob/6.0.3/tests/cases/compiler/typeParameterConstraintInstantiation.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Check that type parameter constraints are properly instantiated

interface Mapper<T> {
    map<U extends T, V extends U[]>(f: (item: T) => U): V;
}

var m: Mapper<string>;
var a = m.map((x: string) => x);  // string[]
//~^ ERROR: Variable 'm' is used before being assigned.
//~| ERROR: Variable 'm' is used before being assigned.
