// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationTypeParameterSyntaxOfClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs

class C<T> { }
//~^ ERROR: Expected '{'.
//~| ERROR: Expected '}'.
//~| ERROR: Cannot find name 'T'.
