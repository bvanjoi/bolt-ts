// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/innerAliases.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export namespace B {
        export namespace C {
            export class Class1 {}
        }
    }
}

namespace D {
    import inner = A.B.C; 
   
    var c1 = new inner.Class1(); 

    export namespace E { 
        export class Class2 {}
    }
}

var c: D.inner.Class1;
//~^ ERROR: Namespace 'D' has no exported member 'inner'.

c = new D.inner.Class1();
//~^ ERROR: Property 'inner' does not exist on type 'typeof D'.

