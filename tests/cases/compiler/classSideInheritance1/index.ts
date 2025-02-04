// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/classSideInheritance1.ts`, Apache-2.0 License

class A {
  static bar(): string {
     return "";
    }
    foo(): number { return 1; }
}
 
class C2 extends A {}

var a: A;
var c: C2;
a.bar(); // static off an instance - should be an error
//~^ ERROR: Property 'bar' does not exist on type 'A'.
c.bar(); // static off an instance - should be an error
//~^ ERROR: Property 'bar' does not exist on type 'C2'.
A.bar(); // valid
C2.bar(); // valid