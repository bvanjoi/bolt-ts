// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationVisibilityErrorTypeLiteral.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

namespace m {
    class private1 {
    }
    namespace m2 {
        export class public1 {
        }
    }

    export var x: {
        x: private1;
        y: m2.public1;
        (): m2.public1[];
        method(): private1;
        [n: number]: private1;
        [s: string]: m2.public1;
    };
    export var x2 = {
        x: new private1(),
        y: new m2.public1(),
        method() {
            return new private1();
        }
    };
    export var x3 = x;
    //~^ ERROR: Variable 'x' is used before being assigned.

    // Function type
    export var y: (a: private1) => m2.public1;
    export var y2 = y;
    //~^ ERROR: Variable 'y' is used before being assigned.

    // constructor type
    export var z: new (a: private1) => m2.public1;
    export var z2 = z;
    //~^ ERROR: Variable 'z' is used before being assigned.
}
