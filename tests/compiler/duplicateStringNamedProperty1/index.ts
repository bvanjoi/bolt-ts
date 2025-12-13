// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/duplicateStringNamedProperty1.ts`, Apache-2.0 License

//@compiler-options: module=commonjs

export interface Album {
    "artist": string;
    artist: string;
    //~^ ERROR: Duplicate identifier 'artist'.
}
