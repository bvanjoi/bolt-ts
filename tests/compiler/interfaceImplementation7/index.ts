// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceImplementation7.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface i1{ name(): { s: string; }; }
interface i2{ name(): { n: number; }; }

interface i3 extends i1, i2 { }
//~^ ERROR: Interface 'i3' cannot simultaneously extend types 'i1' and 'i2'.
interface i4 extends i1, i2 { name(): { s: string; n: number; }; }

class C1 implements i4 {
    public name(): string { return ""; }
//~^ ERROR: Property 'name' in type 'C1<C1>' is not assignable to the same property in base type 'i4'.
}