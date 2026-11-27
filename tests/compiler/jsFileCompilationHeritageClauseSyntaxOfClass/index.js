// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationHeritageClauseSyntaxOfClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs

class C implements D { }
//~^ ERROR: 'implements' clauses can only be used in TypeScript files.
//~| ERROR: Cannot find name 'D'.