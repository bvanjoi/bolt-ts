// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterListAsTupleType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo(a: number, b: string) {
  return true;
}
type Foops = Parameters<typeof foo>;

const x = (a: number) => 5;
type Xps = Parameters<typeof x>;
const a: Xps = ['should-not-work']; // works, but shouldn't
//~^ ERROR: Type 'string' is not assignable to type 'number'.
function t(...args: Xps) {} // should work

class C {
    constructor(a: number, b: string) {
    }
}

type Cps = Parameters<typeof C>; // should not work
//~^ ERROR: Type 'typeof C' does not satisfy the constraint '(args: any) => any'.
type Ccps = ConstructorParameters<typeof C>; // should be [number, string]

class D {
    constructor(a: number, ...rest: string[]) {
    }
}
type Dcps = ConstructorParameters<typeof D>; // should be [number, ...string[]]
