// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing6.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Base {
  constructor(...arg) {}
}
class Super extends Base {
  constructor() {(() => (this));
    super();}
}