// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interface6.ts`, Apache-2.0 License

interface i1 { foo: number; };
interface i2 extends i1 { foo: number; };
interface i3 extends i1 { foo: string; };
//~^ ERROR: Interface 'i3' incorrectly extends interface 'i1'.
interface i4 {
 bar():any;
 bar():any;
}
