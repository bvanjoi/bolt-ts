// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedInterfaceinNamespace5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    interface i1 {

    }

    export interface i2 {

    }

    interface i3 extends i1 {

    }

    export class c1 implements i3 {

    }

    interface i4 {

    }

    export let c2:i4;
}