// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringAssignment_private.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    private x = 0;
    private o = [{ a: 1 }];
}
let x: number;
([{ a: { x } }] = [{ a: new C() }]);
//~^ ERROR: Property 'x' is private and only accessible within class 'C'.
({ o: [{ a: x }]} = new C());
//~^ ERROR: Property 'o' is private and only accessible within class 'C'.
const nameX = "x";
([{ a: { [nameX]: x } }] = [{ a: new C() }]);
//~^ ERROR: Property 'x' is private and only accessible within class 'C'.

const nameO = "o";
({ [nameO]: [{ a: x }]} = new C());
//~^ ERROR: Property 'o' is private and only accessible within class 'C'.
