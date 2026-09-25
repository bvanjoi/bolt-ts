// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/stripMembersOptionality.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@[exactOptionalPropertyTypes=true]  compiler-options: exactOptionalPropertyTypes
//@[exactOptionalPropertyTypes=false] compiler-options: exactOptionalPropertyTypes=false
//@compiler-options: noEmit

declare const someVal: Required<{
    fn?(key: string): string | null;
}>;
someVal.fn("");

declare const someVal2: Required<{
    fn?: (key: string) => string | null;
}>;
someVal2.fn("");
