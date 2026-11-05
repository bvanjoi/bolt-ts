// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/innerExtern.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace A {
    export declare namespace BB {
        export var Elephant;
    }
    export namespace B {
        export class C {
            x = BB.Elephant.X;
        }
    }
}


