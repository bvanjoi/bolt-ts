// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interfaceNameAsIdentifier.ts`, Apache-2.0 License

interface C {
    (): void;
}
C();
//~^ ERROR: Cannot find name 'C'.

namespace m2 {
    export interface C {
        (): void;
    }
}

m2.C();
//~^ ERROR: Cannot find name 'm2'.
