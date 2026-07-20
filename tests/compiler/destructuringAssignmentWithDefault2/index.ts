// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringAssignmentWithDefault2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext]
//@compiler-options: strictNullChecks

const a: { x?: number; y?: number } = { };

let x: number;

// Should not error out
({ x = 0 } = a);
({ x: x = 0} = a);
({ y: x = 0} = a);

// Should be error
({ x = undefined } = a);
//~^ ERROR: Type 'undefined' is not assignable to type 'number'.
({ x: x = undefined } = a);
//~^ ERROR: Type 'undefined' is not assignable to type 'number'.
({ y: x = undefined } = a);
//~^ ERROR: Type 'undefined' is not assignable to type 'number'.
const { x: z1 } = a;
const { x: z2 = 0 } = a;
const { x: z3 = undefined } = a;


declare const r: Iterator<number>;
let done: boolean;
let value;

({ done = false, value } = r.next());
({ done: done = false, value } = r.next());
