// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericOfACloduleType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class G<T>{ bar(x: T) { return x; } }
namespace M {
    export class C { foo() { } }
    export namespace C {
        export class X {
        }
    }
 
    var g1 = new G<C>();
    g1.bar(null).foo();
    //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'M.C'.
}
var g2 = new G<M.C>() // was: error Type reference cannot refer to container 'M.C'.