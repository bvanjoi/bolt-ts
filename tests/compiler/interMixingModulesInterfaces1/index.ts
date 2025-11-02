// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interMixingModulesInterfaces1.ts`, Apache-2.0 License

namespace A {

    export interface B {
        name: string;
        value: number;
    }

    export namespace B {
        export function createB(): B {
            return null;
        }
    }
}

var x: A.B = A.B.createB();