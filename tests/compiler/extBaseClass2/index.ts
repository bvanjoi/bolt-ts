// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/extBaseClass2.ts`, Apache-2.0 License

namespace N {
    export class C4 extends M.B {
        //~^ ERROR: Property 'B' does not exist on type 'typeof M'.
    }
}

namespace M {
    export class C5 extends B {
        //~^ ERROR: Cannot find name 'B'.
    }
}
