
declare global {
  class A {}
  const B: number;
	type C = {foo: string};
  var D: number;
}

const a0: string = B;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
const a1: string = A;
//~^ ERROR: Type 'typeof A' is not assignable to type 'string'.
const a2: C = '42';
//~^ ERROR: Type 'string' is not assignable to type 'C'.
const a3: string = D;
//~^ ERROR: Type 'number' is not assignable to type 'string'.

export {};