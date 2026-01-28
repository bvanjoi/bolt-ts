// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/indexWithoutParamType2.ts`, Apache-2.0 License

class C {
    // Used to be indexer, now it is a computed property
    [x]: string
    //~^ ERROR: Cannot find name 'x'.
}