// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedClassesinModule1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters


namespace A {
    class Calculator {
      //~^ ERROR: 'Calculator' is declared but never used.
        public handelChar() {
        }
    }
}