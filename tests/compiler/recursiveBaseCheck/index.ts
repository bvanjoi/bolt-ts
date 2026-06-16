// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveBaseCheck.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace Module {
    class C extends D {
      //~^ ERROR: 'C' is referenced directly or indirectly in its own base expression.
    }
    export class B extends Module.C {
      //~^ ERROR: 'B' is referenced directly or indirectly in its own base expression.
    }
    export class A extends Module.B {
      //~^ ERROR: 'A' is referenced directly or indirectly in its own base expression.
    }
    export class AmChart extends Module.A {
      //~^ ERROR: 'AmChart' is referenced directly or indirectly in its own base expression.
    }
    export class D extends AmChart {
      //~^ ERROR: 'D' is referenced directly or indirectly in its own base expression.
    }
    export class E extends Module.D {
    }
    export class F extends Module.E {
    }
}
