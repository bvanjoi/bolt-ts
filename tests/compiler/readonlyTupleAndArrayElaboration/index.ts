// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/readonlyTupleAndArrayElaboration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

let point = [3, 4] as const;

function distanceFromOrigin([x, y]: [number, number]) {
    return Math.sqrt(x ** 2 + y ** 2);
}

distanceFromOrigin(point);
//~^ ERROR: Argument of type 'readonly [3, 4]' is not assignable to parameter of type '[number, number]'.

declare function arryFn(x: number[]): void;
arryFn(point);
//~^ ERROR: Argument of type 'readonly [3, 4]' is not assignable to parameter of type 'number[]'.

declare function arryFn2(x: Array<number>): void;
arryFn2(point);
//~^ ERROR: Argument of type 'readonly [3, 4]' is not assignable to parameter of type 'number[]'.

declare const a: readonly number[];
declare const b: Readonly<number[]>;
declare const c: ReadonlyArray<number>;

arryFn2(a);
//~^ ERROR: Argument of type 'readonly number[]' is not assignable to parameter of type 'number[]'.
arryFn2(b);
//~^ ERROR: Argument of type 'readonly number[]' is not assignable to parameter of type 'number[]'.
arryFn2(c);
//~^ ERROR: Argument of type 'readonly number[]' is not assignable to parameter of type 'number[]'.

const t1: readonly [1] = [1];
const t2: readonly [] = t1;
//~^ ERROR: Type 'readonly [1]' is not assignable to type 'readonly []'.

const t3: readonly [1] = [1];
const t4: [] = t3;
//~^ ERROR: Type 'readonly [1]' is not assignable to type '[]'.

const t5: [1] = [1];
const t6: readonly [] = t5;
//~^ ERROR: Type '[1]' is not assignable to type 'readonly []'.

const t7: [1] = [1];
const t8: [] = t7;
//~^ ERROR: Type '[1]' is not assignable to type '[]'.

const a1: readonly number[] = [1];
const a2: readonly boolean[] = a1;
//~^ ERROR: Type 'readonly number[]' is not assignable to type 'readonly boolean[]'.

const a3: readonly number[] = [1];
const a4: boolean[] = a3;
//~^ ERROR: Type 'readonly number[]' is not assignable to type 'boolean[]'.

const a5: number[] = [1];
const a6: readonly boolean [] = a5;
//~^ ERROR: Type 'number[]' is not assignable to type 'readonly boolean[]'.

const a7: number[] = [1];
const a8: boolean[] = a7;
//~^ ERROR: Type 'number[]' is not assignable to type 'boolean[]'.

const ta1: readonly [1] = [1];
const ta2: readonly boolean[] = ta1;
//~^ ERROR: Type 'readonly [1]' is not assignable to type 'readonly boolean[]'.

const ta3: readonly [1] = [1];
const ta4: number[] = ta3;
//~^ ERROR: Type 'readonly [1]' is not assignable to type 'number[]'.

const ta5: [1] = [1];
const ta6: readonly boolean[] = ta5;
//~^ ERROR: Type '[1]' is not assignable to type 'readonly boolean[]'.

const ta7: [1] = [1];
const ta8: boolean[] = ta7;
//~^ ERROR: Type '[1]' is not assignable to type 'boolean[]'.

const at1: readonly number[] = [1];
const at2: readonly [1] = at1;
//~^ ERROR: Type 'readonly number[]' is not assignable to type 'readonly [1]'.

const at3: readonly number[] = [1];
const at4: [1] = at3;
//~^ ERROR: Type 'readonly number[]' is not assignable to type '[1]'.

const at5: number[] = [1];
const at6: readonly [1] = at5;
//~^ ERROR: Type 'number[]' is not assignable to type 'readonly [1]'.

const at7: number[] = [1];
const at8: [1] = at7;
//~^ ERROR: Type 'number[]' is not assignable to type '[1]'.