// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interMixingModulesInterfaces0.ts`, Apache-2.0 License

namespace A {

    export namespace B {
        export function createB(): B {
            return null;
        }
    }

    export interface B {
        name: string;
        value: number;
    }
}

var x: A.B = A.B.createB();