// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumLiteralAssignableToEnumInsideUnion.ts`, Apache-2.0 License

//@compiler-options: target=es6

namespace X {
    export enum Foo {
        A, B
    }
}
namespace Y {
    export enum Foo {
        A, B
    }
}
namespace Z {
    export enum Foo {
        A = 1 << 1,
        B = 1 << 2,
    }
}
namespace Ka {
    export enum Foo {
        A = 1 << 10,
        B = 1 << 11,
    }
}
const e0: X.Foo | boolean = Y.Foo.A; // ok
const e1: X.Foo | boolean = Z.Foo.A; // not legal, Z is computed
//~^ ERROR: Type 'Foo' is not assignable to type 'false | true | Foo.A | Foo.B'.
const e2: X.Foo.A | X.Foo.B | boolean = Z.Foo.A; // still not legal
//~^ ERROR: Type 'Foo' is not assignable to type 'false | true | Foo.A | Foo.B'.
const e3: X.Foo.B | boolean = Z.Foo.A; // not legal
//~^ ERROR: Type 'Foo' is not assignable to type 'false | true | Foo.B'.
const e4: X.Foo.A | boolean = Z.Foo.A; // not legal either because Z.Foo is computed and Z.Foo.A is not necessarily assignable to X.Foo.A
//~^ ERROR: Type 'Foo' is not assignable to type 'false | true | Foo.A'.
const e5: Ka.Foo | boolean = Z.Foo.A; // ok
//~^ ERROR: Type 'Foo' is not assignable to type 'false | true | Foo.A | Foo.B'.
