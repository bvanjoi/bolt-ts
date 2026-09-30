// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationTypeParameterSyntaxOfClassExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs
//@compiler-options: noEmit

const Bar = class<T> {};
//~^ ERROR: Expected '{'.
//~| ERROR: Expected '}'.
//~| ERROR: Cannot find name 'T'.
