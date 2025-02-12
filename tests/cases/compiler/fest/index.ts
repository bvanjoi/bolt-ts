// From `github.com/sindresorhus/type-fest`, MIT License

function n(): never {
  throw new Error();
}

// =========== And ===========
type And<A extends boolean, B extends boolean> = [A, B][number] extends true
	? true
	: true extends [IsEqual<A, false>, IsEqual<B, false>][number]
		? false
		: never;
{
  const never: never = n();

  const a1: And<true, true> = true;
  const a2: And<true, false> = false;
  const a3: And<false, true> = false;
  const a4: And<false, false> = false;
  const a5: And<true, boolean> = never;
  const a6: And<false, boolean> = false;
  const a7: And<boolean, boolean> = never;
}

// ========= ArrayIndices =========
type ArrayIndices<Element extends readonly unknown[]> =
	Exclude<Partial<Element>['length'], Element['length']>;
{
  const values = ['a', 'b', 'c'] as const;
  type ValueKeys = ArrayIndices<typeof values>;

  const test: 0 | 1 | 2 = 0;
  // const a0: ValueKeys = test;

  // expectAssignable<ValueKeys>(0);
  // expectAssignable<ValueKeys>(1);
  // expectAssignable<ValueKeys>(2);

  // expectNotAssignable<ValueKeys>(-1);
  // expectNotAssignable<ValueKeys>(3);

  // type TupleKeys = ArrayIndices<['a', 2]>;

  // declare const testTuple: 0 | 1;
  // expectType<TupleKeys>(testTuple);

  // expectAssignable<TupleKeys>(0);
  // expectAssignable<TupleKeys>(1);

  // expectNotAssignable<TupleKeys>(-1);
  // expectNotAssignable<TupleKeys>(2);
}

// ========= ArrayValues =========
type ArrayValues<T extends readonly unknown[]> = T[number];
{
  const values = ['a', 'b', 'c'] as const;
  type Values = ArrayValues<typeof values>;

  const test: 'a' | 'b' | 'c' = 'a';
  const a0: Values = test;

  const a1: Values = 'a';
  const a2: Values = 'b';
  const a3: Values = 'c';

  const a4: Values = '';
  //~^ ERROR: Type 'string' is not assignable to type '"a" | "b" | "c"'.
  const a5: Values = 0;
  //~^ ERROR: Type 'number' is not assignable to type '"a" | "b" | "c"'.

  type TupleValues = ArrayValues<['1', 2, {c: true}]>;

  const testTuple: '1' | 2 | {c: true} = '1';
  const b0: TupleValues = testTuple;

  const b1: TupleValues = '1';
  const b2: TupleValues = 2;
  const b3: TupleValues = {c: true};

  const b4: TupleValues = {};
  //~^ ERROR: Type '{ }' is not assignable to type '2 | "1" | { c: true; }'.
  const b5: TupleValues = 1;
  //~^ ERROR: Type 'number' is not assignable to type '2 | "1" | { c: true; }'.
  const b6: TupleValues = '2';
  //~^ ERROR: Type 'string' is not assignable to type '2 | "1" | { c: true; }'.

  type AnyStringValues = ArrayValues<string[]>;
  const c0: AnyStringValues = '';
  const c1: AnyStringValues = '123';

  const c2: AnyStringValues = 123;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  const c3: AnyStringValues = undefined; // Depends on `strictNullChecks`
  const c4: AnyStringValues = null;      // Depends on `strictNullChecks`
}

// ========== Arrayable ==========
type Arrayable<T> = T | T[];
{
  const unknown: unknown = 'unknown';
  const a0: Arrayable<string> = unknown as string | string[];
  const a1: Arrayable<string | {foo: number}> = unknown as (string | {foo: number}) | Array<string | {foo: number}>;
  const a2: Arrayable<never> = unknown as /* never | */ never[];
  const a3: Arrayable<string[]> = unknown as string[] | string[][];

  // Test for issue https://github.com/sindresorhus/type-fest/issues/952
  type Item = number;
  function castArray1(value: Arrayable<Item>): Item[] {
    return Array.isArray(value) ? value : [value];
  }

  const b0: Item[] = unknown as ReturnType<typeof castArray1>;

  function castArray2<T>(value: Arrayable<T>): T[] {
    return Array.isArray(value) ? value : [value];
  }

  const c0: number[] = castArray2(1);
  const c1: number[] = castArray2([1, 2, 3]);
}

// =========== ifNever ===========
type IfNever<T, TypeIfNever = true, TypeIfNotNever = false> = (
	IsNever<T> extends true ? TypeIfNever : TypeIfNotNever
);

{
  const a0: IfNever<never> = true;
  const a1: IfNever<string> = false;
  const a2: IfNever<never, 'T', 'F'> = 'T';
  const a3: IfNever<string, 'T', 'F'> = 'F';

  type A = IfNever;
  //~^ ERROR: Generic type 'IfNever' requires between 1 and 3 type arguments.
}

// =========== ifNull ===========
type IfNull<T, TypeIfNull = true, TypeIfNotNull = false> = (
	IsNull<T> extends true ? TypeIfNull : TypeIfNotNull
);

// =========== IsEqual ===========
type IsEqual<A, B> =
	(<G>() => G extends A & G | G ? 1 : 2) extends
	(<G>() => G extends B & G | G ? 1 : 2)
		? true
		: false;

{
  const notEqualNumberAndString: IsEqual<number, string> = false;
  const a0: false = notEqualNumberAndString;

  const equalNumbers: IsEqual<1, 1> = true;
  const a1: true = equalNumbers;

  const notEqualAnyAndNumber: IsEqual<any, number> = false;
  const a2: false = notEqualAnyAndNumber;

  const notEqualUnionAndNumber: IsEqual<1 | 2, 1> = false;
  const a3: false = notEqualUnionAndNumber;

  const notEqualAnyAndNever: IsEqual<any, never> = false;
  const a4: false = notEqualAnyAndNever;

  const notEqualArrayOfAnyAndArrayOfNever: IsEqual<[any], [never]> = false;
  const a5: false = notEqualArrayOfAnyAndArrayOfNever;

  type A = IsEqual;
	//~^ ERROR: Generic type 'IsEqual' requires 2 type arguments.

  type B = IsEqual<number>;
	//~^ ERROR: Generic type 'IsEqual' requires 2 type arguments.

  type UnionType0 = IsEqual<{a: 1}, {a: 1}>;
  const a60: UnionType0 = true;

	type UnionType1 = IsEqual<{a: 1} & {a: 1}, {a: 1}>;
  const a61: UnionType1 = true;

  type UnionType2 = IsEqual<{a: 1} & {a: 2}, {a: 1}>;
  const a62: UnionType2 = false;

  type IntersectionType = IsEqual<{a: 1} | {a: 1}, {a: 1}>;
  const a7: IntersectionType = true;


  const a80: [IsEqual<true, false>, IsEqual<false, false>][number] = true;
  const a81: [IsEqual<true, false>, IsEqual<false, false>][number] = false;
}

// =========== isNever ===========
type IsNever<T> = [T] extends [never] ? true : false;

{
  const _never: never = n();
  const something = 'something';
  
  // `IsNever` should only be true for `any`
  const a0: IsNever<never> = true;
  const a1: IsNever<typeof _never> = true;
  const a2: IsNever<string> = false;
  const a3: IsNever<typeof something> = false;
  const a4: IsNever<any> = false;
  const a5: IsNever<unknown> = false;
  const a6: IsNever<null> = false;
  const a7: IsNever<undefined> = false;
  const a8: IsNever<void> = false;
  
  type A0 = IsNever;
  //~^ ERROR: Generic type 'IsNever' requires 1 type argument.
  type A1 = IsNever<number, boolean>;
  //~^ ERROR: Generic type 'IsNever' requires 1 type argument.
}

// =========== isNull ===========
type IsNull<T> = [T] extends [null] ? true : false;

{
  const a0: IsNull<null> = true;
  const a1: IsNull<any> = true;
  const a2: IsNull<never> = true;
  const a3: IsNull<undefined> = true; // Depends on `strictNullChecks`
  const a4: IsNull<unknown> = false;
  const a5: IsNull<void> = false;
  const a6: IsNull<{}> = false;
}

// =========== isUnknown ===========
type IsUnknown<T> = (
	unknown extends T // `T` can be `unknown` or `any`
		? IsNull<T> extends false // `any` can be `null`, but `unknown` can't be
			? true
			: false
		: false
);
{
  const _unknown: unknown = 'unknown';
  const something = 'something';

  // `IsUnknown` should only be true for `any`
  const a0: IsUnknown<unknown> = true;
  const a1: IsUnknown<typeof _unknown> = true;
  const a2: IsUnknown<string> = false;
  const a3: IsUnknown<typeof something> = false;
  const a4: IsUnknown<any> = false;
  const a5: IsUnknown<never> = false;
  const a6: IsUnknown<null> = false;
  const a7: IsUnknown<undefined> = false;
  const a8: IsUnknown<void> = false;

  type A = IsUnknown;
  //~^ ERROR: Generic type 'IsUnknown' requires 1 type argument.
}

// =============== Or ===============
type Or<A extends boolean, B extends boolean> = [A, B][number] extends false
	? false
	: true extends [IsEqual<A, true>, IsEqual<B, true>][number]
		? true
		: never;

{
  const never: never = n();

  const a0: Or<true, true> = true;
  const a1: Or<true, false> = true;
  const a2: Or<false, true> = true;
  const a3: Or<false, false> = false;
  const a4: Or<true, boolean> = true;
  const a5: Or<false, boolean> = never;
  const a6: Or<boolean, boolean> = never;
}

// =========== UnknownArray ===========
type UnknownArray = readonly unknown[];
{
  const foo: readonly [] = [];
  const bar: {
    readonly array: unknown[]
  } = { array: [] };

  const a0: UnknownArray = foo;
  const a1: UnknownArray = bar.array;
  const a2: UnknownArray = [];
  const a3: UnknownArray = ['foo'];

    const b0: UnknownArray = null;      // depend on `strictNullChecks`
    const b1: UnknownArray = undefined; // depend on `strictNullChecks`
    const b2: UnknownArray = {};
    //~^ ERROR: Type '{ }' is missing the following properties from type 'unknown[]': length, join, and 
    const b3: UnknownArray = {0: 1};
    //~^ ERROR: Type '{ 0: number; }' is missing the following properties from type 'unknown[]': length, join, and 
    const b4: UnknownArray = 1;
    //~^ ERROR: Type 'number' is not assignable to type 'unknown[]'.
    const b5: UnknownArray = Date;
    //~^ ERROR: Type 'DateConstructor' is missing the following properties from type 'unknown[]': join, reverse, and
    //~| ERROR: Type 'DateConstructor' is not assignable to type 'unknown[]'.

    type IsArray<T> = T extends UnknownArray ? true : false;

    const string: IsArray<string> = false;
    const array: IsArray<[]> = true;
    const tuple: IsArray<['foo']> = true;
    const readonlyArray: IsArray<readonly number[]> = true;
    const leadingSpread: IsArray<readonly [number, ...string[]]> = true;
    const trailingSpread: IsArray<readonly [...string[], number]> = true;
}
