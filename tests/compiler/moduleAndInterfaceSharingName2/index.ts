// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/moduleAndInterfaceSharingName2.ts`, Apache-2.0 License

namespace X {
    export namespace Y {
        export interface Z { }
    }
    export interface Y { }
}
var z: X.Y.Z = null;
//~^ ERROR: Type 'null' is not assignable to type 'X.Y.Z'.
var z2: X.Y<string>;
//~^ ERROR: Type 'Y' is not generic.
