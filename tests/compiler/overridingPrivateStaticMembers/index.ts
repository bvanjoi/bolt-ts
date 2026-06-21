// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overridingPrivateStaticMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base2 {
    private static y: { foo: string };
}
 
class Derived2 extends Base2 {
  //~^ ERROR: Class static side 'typeof Derived2' incorrectly extends base class static side 'typeof Base2'.
    private static y: { foo: string; bar: string; };
}