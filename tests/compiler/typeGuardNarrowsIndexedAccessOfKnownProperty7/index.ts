// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty7.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext

export namespace Foo {
    export const key = Symbol();
}

export class C {
    [Foo.key]: string;

    constructor() {
        this[Foo.key] = "hello";
    }
}