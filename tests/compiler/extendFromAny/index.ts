// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/extendFromAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var Base: any;
class C extends Base {
    known = 1;
    static sknown = 2;
}

let c = new C();
c.known.length; // error, 'known' has no 'length' property
//~^ ERROR: Property 'length' does not exist on type 'number'.
C.sknown.length; // error, 'sknown' has no 'length' property
//~^ ERROR: Property 'length' does not exist on type 'number'.
c.unknown.length; // ok, unknown: any
C.sunknown.length; // ok: sunknown: any
