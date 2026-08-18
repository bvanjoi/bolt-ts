// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticMemberExportAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class Sammy {
   foo() { return "hi"; }
  static bar() {
    return -1;
   }
}
namespace Sammy {
    export var x = 1;
}
interface JQueryStatic {
    sammy: Sammy; // class instance
}
declare var $: JQueryStatic;
var instanceOfClassSammy: Sammy = new $.sammy(); // should be error
//~^ ERROR: This expression is not constructable.
var r1 = instanceOfClassSammy.foo(); // r1 is string
var r2 = $.sammy.foo();
var r3 = $.sammy.bar(); // error
//~^ ERROR: Property 'bar' does not exist on type 'Sammy'.
var r4 = $.sammy.x; // error
//~^ ERROR: Property 'x' does not exist on type 'Sammy'.

Sammy.bar();