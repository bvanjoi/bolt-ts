// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericArrayMethods1.ts`, Apache-2.0 License

var x:string[] =  [0,1].slice(0);
//~^ ERROR: Type 'number[]' is not assignable to type 'string[]'.

{
  let a0: [] = [];
}

{
	type ArrayTail<TArray extends unknown[]> = TArray extends [unknown?, ...infer Tail]
	? keyof TArray & `${number}` extends never
		? []
		: Tail
	: [];
	const a: ArrayTail<[]> = ['a'];
	//~^ ERROR: Type '[string]' is not assignable to type '[]'.

	let b: readonly [number, ...number[]] = {} as Readonly<[number, ...string[]]>;
	//~^ ERROR: Type 'readonly [number, ...string[]]' is not assignable to type 'readonly [number, ...number[]]'.
}

{
	type A = keyof [100000000000,2,3]
	let e: A = 0;
	let e0: A = '0';
	let a: A = 1;
	let a0: A = '1'
	let b: A = 2;
	let b0: A = '2'
	let c: A = 3;
	let d: A = 4;
	let c0: A = '3';
	//~^ ERROR: Type 'string' is not assignable to type
}

{
	type A<TArray extends unknown[]> = (keyof TArray & `${number}`) extends never ? true : false;
	const a: A<[1, 2, ...number[]]> = true;
	//~^ ERROR: Type 'boolean' is not assignable to type 'false'.
}