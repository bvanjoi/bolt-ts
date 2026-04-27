// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/externSemantics.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare var x=10; //~ERROR: Initializers are not allowed in ambient contexts.
declare var v;
declare var y:number=3; //~ERROR: Initializers are not allowed in ambient contexts.
declare const k = 42;

namespace N {
  declare var a = 0; //~ERROR: Initializers are not allowed in ambient contexts.
}


declare namespace M {
  abstract class C {
    readonly a = "a"
  }
  abstract class C2 {
    a = "a" //~ERROR: Initializers are not allowed in ambient contexts.
  }
  class D {
    readonly a = "a"
  }
  abstract class D2 {
    a = "a" //~ERROR: Initializers are not allowed in ambient contexts.
  }
}