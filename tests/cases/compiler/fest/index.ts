// From `github.com/sindresorhus/type-fest`, MIT License

// type ArrayValues<T extends readonly unknown[]> = T[number];
// {
//   // const values = ['a', 'b', 'c'] as const;
//   // type Values = ArrayValues<typeof values>;

//   // const test: 'a' | 'b' | 'c' = 'a';
//   // const a0: Values = test;

//   // const a1: Values = 'a';
//   // const a2: Values = 'b';
//   // const a3: Values = 'c';

//   // const a4: Values = '';
//   // const a5: Values = 0;

//   // type TupleValues = ArrayValues<['1', 2, {c: true}]>;

//   // declare const testTuple: '1' | 2 | {c: true};
//   // expectType<TupleValues>(testTuple);

//   // expectAssignable<TupleValues>('1');
//   // expectAssignable<TupleValues>(2);
//   // expectAssignable<TupleValues>({c: true});

//   // expectNotAssignable<TupleValues>({});
//   // expectNotAssignable<TupleValues>(1);
//   // expectNotAssignable<TupleValues>('2');

//   // type AnyStringValues = ArrayValues<string[]>;
//   // expectAssignable<AnyStringValues>('');
//   // expectAssignable<AnyStringValues>('123');
//   // expectNotAssignable<AnyStringValues>(123);
//   // expectNotAssignable<AnyStringValues>(undefined);
//   // expectNotAssignable<AnyStringValues>(null);
// }


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

// ========= ArrayValues =========

type ArrayValues<T extends readonly unknown[]> = T[number];

{
  const values = ['a', 'b', 'c'] as const;
  // type Values = ArrayValues<typeof values>;

  // const test: 'a' | 'b' | 'c' = 'a';
  // const a0: Values = test;

  // const a1: Values = 'a';
  // const a2: Values = 'b';
  // const a3: Values = 'c';

  // const a4: Values = '';
  // const a5: Values = 0;

  // type TupleValues = ArrayValues<['1', 2, {c: true}]>;

  // declare const testTuple: '1' | 2 | {c: true};
  // expectType<TupleValues>(testTuple);

  // expectAssignable<TupleValues>('1');
  // expectAssignable<TupleValues>(2);
  // expectAssignable<TupleValues>({c: true});

  // expectNotAssignable<TupleValues>({});
  // expectNotAssignable<TupleValues>(1);
  // expectNotAssignable<TupleValues>('2');

  // type AnyStringValues = ArrayValues<string[]>;
  // expectAssignable<AnyStringValues>('');
  // expectAssignable<AnyStringValues>('123');
  // expectNotAssignable<AnyStringValues>(123);
  // expectNotAssignable<AnyStringValues>(undefined);
  // expectNotAssignable<AnyStringValues>(null);
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
