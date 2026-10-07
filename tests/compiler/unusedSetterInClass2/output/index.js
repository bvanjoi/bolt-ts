// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedSetterInClass2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
class Employee {
  set p(_) {}
  m() {
    this.p = 0;
  }
}