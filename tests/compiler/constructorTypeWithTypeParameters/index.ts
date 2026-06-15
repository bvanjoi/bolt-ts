// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorTypeWithTypeParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

declare var X: {
    new <T>(): number;
}
declare var Y: {
    new (): number;
}
var anotherVar: new <T>() => number;
