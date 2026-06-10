// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/computedPropertyNameAndTypeParameterConflict.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

declare const O: unique symbol;
declare class Bar<O> {
    [O]: number;
}

