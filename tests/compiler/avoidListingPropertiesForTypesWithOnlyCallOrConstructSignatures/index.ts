// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/avoidListingPropertiesForTypesWithOnlyCallOrConstructSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Dog {
    barkable: true
}

declare function getRover(): Dog
    
export let x:Dog = getRover;  //~ERROR: Type '() => Dog' is not assignable to type 'Dog'.
// export let x: Dog = getRover; 