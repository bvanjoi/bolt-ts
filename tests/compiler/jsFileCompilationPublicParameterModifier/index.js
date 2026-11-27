// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationPublicParameterModifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs

class C { constructor(public x) { }}
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: The 'public' modifier can only be used in TypeScript files.
