// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/moduleAndInterfaceSharingName3.ts`, Apache-2.0 License

namespace X {
    export namespace Y {
        export interface Z { }
    }
    export interface Y<T> { }
}
var z: X.Y.Z = null;
var z2: X.Y<string>;