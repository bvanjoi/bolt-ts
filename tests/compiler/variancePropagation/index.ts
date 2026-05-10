// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/variancePropagation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface DerivedTable<S extends { base: any; new: any }> {
    // Error disappears when these property declarations are reversed
    schema: S["base"] & S["new"]
    readonlySchema: Readonly<S["base"] & S["new"]>
}

interface Base { baseProp: number; }
interface New  { newProp: boolean; }

declare const source: DerivedTable<{ base: Base, new: New }>
const destination: DerivedTable<{ base: Base; new: New & Base }> = source; // Error
