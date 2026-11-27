// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationPublicMethodSyntaxOfClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs

class C {
    public foo() {
      //~^ ERROR: The 'public' modifier can only be used in TypeScript files.
    }
}

class C0 {
    protected foo() {
      //~^ ERROR: The 'protected' modifier can only be used in TypeScript files.
    }
}

class C1 {
    private foo() {
      //~^ ERROR: The 'private' modifier can only be used in TypeScript files.
    }
}