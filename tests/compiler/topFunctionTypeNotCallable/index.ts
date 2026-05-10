// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/topFunctionTypeNotCallable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare let foo: (...args: never) => void;
foo(); // error
//~^ ERROR: Argument of type '[]' is not assignable to parameter of type 'never'.

{
	type A<N> =
		N extends unknown[]
			? N extends [1n]
					? '0' extends N
						? 1 
						: 2
					: 3
			: 4;

	const a: A<[1n]> = 42;
	//~^ ERROR: Type '42' is not assignable to type '2'.
}

{
	type E<A, B> = [B] extends [A] ? 1 : 2;
	const e: E<[{a: 1}] & [{a: 1}], [{a : 1}]> = 42;
	//~^ ERROR: Type '42' is not assignable to type '1'.
}