// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExtendsNull2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

interface Base {}

class C extends null {
  //~^ ERROR: Class static side 'typeof C' incorrectly extends base class static side 'null'.
  constructor() {
    super();
    //~^ ERROR: A constructor cannot contain a 'super' call when its class extends 'null'.
  }
}
interface C extends Base {}
