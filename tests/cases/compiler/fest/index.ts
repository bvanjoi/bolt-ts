// From `github.com/sindresorhus/type-fest`, MIT License

// type IsEqual<A, B> =
// 	(<G>() => G extends A & G | G ? 1 : 2) extends
// 	(<G>() => G extends B & G | G ? 1 : 2)
// 		? true
// 		: false;

// type WritableKeysOf<T> = {
// 	[P in keyof T]: IsEqual<{[Q in P]: T[P]}, {readonly [Q in P]: T[P]}> extends false ? P : never
// };

// {
//   type TestType1 = {
//     readonly a: string;
//     b: boolean;
//   };
  
//   // type TestType2 = {
//   //   a: string;
//   //   b: boolean;
//   // };
  
//   // type TestType3 = {
//   //   readonly a: string;
//   //   readonly b: boolean;
//   // };

//   type WritableKeysOf1 = WritableKeysOf<TestType1>;
//   // type WritableKeysOf2 = WritableKeysOf<TestType2>;
//   // type WritableKeysOf3 = WritableKeysOf<TestType3>;
  
//   const test1: WritableKeysOf1['b'] = 'c';
//   // const test2: WritableKeysOf1 = 'a';
//   // const test2: WritableKeysOf2 = 'b';
//   // const test3: WritableKeysOf2 = 'a';
//   // const test4: WritableKeysOf3 = n();
  
//   // const a0: 'b' = test1;
//   // const a1: 'a' | 'b' = test2;
//   // const a2: never = test4;
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

// ========= ArrayIndices =========
type ArrayIndices<Element extends readonly unknown[]> =
	Exclude<Partial<Element>['length'], Element['length']>;
{
  const values = ['a', 'b', 'c'] as const;
  type ValueKeys = ArrayIndices<typeof values>;

  const test: 0 | 1 | 2 = 0;
  const a0: ValueKeys = test;

  const a1:ValueKeys = 0;
  const a2:ValueKeys = 1;
  const a3:ValueKeys = 2;

  const a4: ValueKeys = -1;
  //~^ ERROR: Type 'number' is not assignable to type '2 | 1 | 0'.
  const a5: ValueKeys = 3;
  //~^ ERROR: Type 'number' is not assignable to type '2 | 1 | 0'.

  type TupleKeys = ArrayIndices<['a', 2]>;

  const testTuple: 0 | 1 = 0;
  const b0: TupleKeys = testTuple;

  const b1: TupleKeys = 0;
  const b2: TupleKeys = 1;

  const b3: TupleKeys = -1;
  //~^ ERROR: Type 'number' is not assignable to type '1 | 0'.
  const b4: TupleKeys = 2;
  //~^ ERROR: Type 'number' is not assignable to type '1 | 0'.

	const c0: ArrayIndices<['a', 'b', 'c']> = 0;
	const c1: ArrayIndices<['a', 'b', 'c']> = 1;
	const c2: ArrayIndices<['a', 'b', 'c']> = 2;
	const c3: ArrayIndices<['a', 'b', 'c']> = 3;
	//~^ ERROR: Type 'number' is not assignable to type '2 | 1 | 0'.
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

// ============ IfAny ============
type IfAny<T, TypeIfAny = true, TypeIfNotAny = false> = (
	IsAny<T> extends true ? TypeIfAny : TypeIfNotAny
);
{
  // `IfAny` should return `true`/`false` if only `T` is specified
  const a0: IfAny<any> = true;
  const a1: IfAny<string> = false;
  const a2: IfAny<any, 'T', 'F'> = 'T';
  const a3: IfAny<string, 'T', 'F'> = 'F';

  type A = IfAny;
  //~^ ERROR: Generic type 'IfAny' requires between 1 and 3 type arguments.
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

// =========== IfUnknown ===========
type IfUnknown<T, TypeIfUnknown = true, TypeIfNotUnknown = false> = (
	IsUnknown<T> extends true ? TypeIfUnknown : TypeIfNotUnknown
);
{
  const a0: IfUnknown<unknown> = true;
  const a1: IfUnknown<string> = false;
  const a2: IfUnknown<unknown, 'T', 'F'> = 'T';
  const a3: IfUnknown<string, 'T', 'F'> = 'F';

  type A = IfUnknown;
  //~^ ERROR: Generic type 'IfUnknown' requires between 1 and 3 type arguments.
}

// =========== IsAny ===========
// Can eventually be replaced with the built-in once this library supports
// TS5.4+ only. Tracked in https://github.com/sindresorhus/type-fest/issues/848
type NoInfer<T> = T extends infer U ? U : never;
type IsAny<T> = 0 extends 1 & NoInfer<T> ? true : false;

{
  const anything: any = 1;
  const something = 'something';
  
  // `IsAny` should only be true for `any`
  const a0: IsAny<any> = true;
  const a1: IsAny<typeof anything> = true;
  const a2: IsAny<string> = false;
  const a3: IsAny<typeof something> = false;
  const a4: IsAny<never> = false;
  const a5: IsAny<unknown> = false;
  const a6: IsAny<null> = false;
  const a7: IsAny<undefined> = false;
  const a8: IsAny<void> = false;
  
  type A = IsAny;
  //~^ ERROR: Generic type 'IsAny' requires 1 type argument.
  
  // Verify that are no circular reference issues
  // https://github.com/sindresorhus/type-fest/issues/846
  type OnlyAny<T extends IsAny<T> extends true ? any : never> = T;
  type B = OnlyAny<any>;
  type C = OnlyAny<string>;
  //~^ ERROR: Type 'string' is not assignable to type 'never'.
}

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

// ============ Simplify ============
type Simplify<T> = {[KeyType in keyof T]: T[KeyType]} & {};
{
  type PositionProperties = {
    top: number;
    left: number;
  };
  
  type SizeProperties = {
    width: number;
    height: number;
  };
  
  // Flatten the type output to improve type hints shown in editors.
  const flattenProperties = {top: 120, left: 240, width: 480, height: 600};
  const a0: Simplify<PositionProperties & SizeProperties> = flattenProperties;
  
  interface SomeInterface {
    foo: number;
    bar?: string;
    baz: number | undefined;
  }
  
  type SomeInterfaceAsTypeWrittenByHand = {
    foo: number;
    bar?: string;
    baz: number | undefined;
  };
  
  const valueAsLiteral = {foo: 123, bar: 'hello', baz: 456};
  const valueAsSimplifiedInterface: Simplify<SomeInterface> = valueAsLiteral;
  const valueAsInterface: SomeInterface = valueAsLiteral;
  
  const a: Simplify<SomeInterface> = valueAsSimplifiedInterface;
  const a1: SomeInterfaceAsTypeWrittenByHand = a;
  
  // Interface is assignable to its Simplified type (created with Simplify, and by hand)
  const a2: Simplify<SomeInterface> = valueAsInterface;
  const a3: SomeInterfaceAsTypeWrittenByHand = valueAsInterface;
  
  // The following demonstrates one reason a type may be preferred over an interface is that it can be assigned to alternate types. In this example the interface cannot be because it is not sealed and elsewhere a non-string property could be added.
  const a4: Record<string, unknown> = valueAsLiteral;
  const a5: Record<string, unknown> = valueAsSimplifiedInterface;
  const a6: Record<string, unknown> = valueAsInterface; // Index signature is missing in interface
  //~^ ERROR: Type 'SomeInterface' is not assignable to type 'Record'. 

  // The following tests should be fixed once we have determined the cause of the bug reported in https://github.com/sindresorhus/type-fest/issues/436
  
  type SomeFunction = (type: string) => string;
  type SimplifiedFunction = Simplify<SomeFunction>; // Return '{}' expected 'SomeFunction'
  
  const someFunction: SimplifiedFunction = {};
  
  const b0: SomeFunction = someFunction;
  //~^ ERROR: Type 'mapped type' is not assignable to type '(type: string) => string'.
}

// ========== TupleToUnion ==========
type TupleToUnion<ArrayType> = ArrayType extends readonly unknown[] ? ArrayType[number] : never;
{
  const options = ['a', 'b', 'c'] as const;
  type Options = TupleToUnion<typeof options>;

  const a: Options = 'a';
  const a1: 'a' = a;
  const a2: TupleToUnion<['a', 'b', 'c']> = 'a';
  const a3: 'b' = a;
  //~^ ERROR: Type '"a"' is not assignable to type '"b"'.
  const a4: 'c' = a;
  //~^ ERROR: Type '"a"' is not assignable to type '"c"'.

  const b: Options = 'b';
  const b0: Options = b;
  const b1: 'a' = b;
  //~^ ERROR: Type '"b"' is not assignable to type '"a"'.
  const b2: 'b' = b;
  const b3: 'c' = b;
  //~^ ERROR: Type '"b"' is not assignable to type '"c"'.

  const c: Options = 'c';
  const c0: Options = c;
  const c1: 'a' = c;
  //~^ ERROR: Type '"c"' is not assignable to type '"a"'.
  const c2: 'b' = c;
  //~^ ERROR: Type '"c"' is not assignable to type '"b"'.
  const c3: 'c' = c;

  const notAnArray: TupleToUnion<[]> = n();
  const d0: never = notAnArray;

  const notAnArray2: TupleToUnion<[]> = undefined;
  //~^ ERROR: Type 'undefined' is not assignable to type 'never'.

  const worksWithArrays: TupleToUnion<Array<string | number>> = 1;
  const e0: string | number  = worksWithArrays;

  const resolvesToNeverForNonArrays: TupleToUnion<string | number> = n();
  const f0: never = resolvesToNeverForNonArrays;

  const infiniteRestArguments: TupleToUnion<[string, ...number[]]> = 1;
  const g0: string | number = infiniteRestArguments
}

// ======= UnionToIntersection =======
type UnionToIntersection<Union> = (
	// `extends unknown` is always going to be the case and is used to convert the
	// `Union` into a [distributive conditional
	// type](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html#distributive-conditional-types).
	Union extends unknown
		// The union type is used as the only argument to a function since the union
		// of function arguments is an intersection.
		? (distributedUnion: Union) => void
		// This won't happen.
		: never
		// Infer the `Intersection` type since TypeScript represents the positional
		// arguments of unions of functions as an intersection of the union.
) extends ((mergedIntersection: infer Intersection) => void)
	// The `& Union` is to allow indexing by the resulting type
	? Intersection & Union
	: never;
{
  const intersection1: UnionToIntersection<{a: string} | {b: number}> = {a: '42', b: 42 }; 
  const a0: {a: string; b: number} = intersection1;

  const intersection2: UnionToIntersection<{a: string} | {b: number} | {a: () => void}> = {a: n(), b: 42};
  const b0: {a: string | (() => void); b: number} = intersection2;

  type ObjectsUnion = {a: string; z: string} | {b: string; z: string} | {c: string; z: string};
  let t: keyof ObjectsUnion = 'a';
  //~^ ERROR: Type '"a"' is not assignable to type '"z"'.
  const value: ObjectsUnion[UnionToIntersection<keyof ObjectsUnion>] = '';
  const c0: string = value;
}

// =========== UnionToTuple ===========
type LastOfUnion<T> =
UnionToIntersection<T extends any ? () => T : never> extends () => (infer R)
	? R
	: never;

{
	let a0: LastOfUnion<'a'> = 'a';
	let a1: LastOfUnion<'a' | 'b'> = 'b';
	let a2: LastOfUnion<'a' | 'b' | 'c'> = 'c';
	let a3: LastOfUnion<'a' | 'b' | 'c'> = 'b';
	//~^ ERROR: Type '"b"' is not assignable to type '"c"'.
	let a4: LastOfUnion<'a' | 'b' | 'c'> = 'd';
	//~^ ERROR: Type '"d"' is not assignable to type '"c"'.
}

type UnionToTuple<T, L = LastOfUnion<T>> =
IsNever<T> extends false
	? [...UnionToTuple<Exclude<T, L>>, L]
	: [];

{
	let b0: UnionToTuple<'a'> = ['a'];
	let b1: UnionToTuple<'a'> = [];
	//~^ ERROR: Type '[]' is not assignable to type '["a"]'.
	type Options = UnionToTuple<'a' | 'b' | 'c'>;
	// Results unordered
	const a0: ['a', 'b', 'c'] | ['a', 'c', 'b'] | ['b', 'a', 'c'] | ['b', 'c', 'a'] | ['c', 'a', 'b'] | ['c', 'b', 'a'] = {} as Options;
	const a1: Options[number] = 'a' as ('a' | 'b' | 'c');

	type Options1 = UnionToTuple<1 | 2 | 3>;
	const a2: Options1[number] = 1 as (1 | 2 | 3)

	type Options2 = UnionToTuple<boolean | 1>;
	const a3: Options2[number] = 1 as (1 | false | true);
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

// =========== UnknownRecord ===========
type UnknownRecord = Record<PropertyKey, unknown>;
{
  let foo: UnknownRecord = {};

  const a0: UnknownRecord = foo;
  const a1: UnknownRecord = foo = {};
  const a2: UnknownRecord = foo = {bar: 'baz'};
  const a3: UnknownRecord = foo = {bar: {baz: 'hello'}};

  foo = [];
  //~^ ERROR: Type 'never[]' is not assignable to type 'Record'.
  foo = 42;
  //~^ ERROR: Type 'number' is not assignable to type 'Record'.
  foo = null; // Depends on `strictNullChecks`

  const b0: unknown = foo['bar'];
}

// =========== ValueOf ===========
type ValueOf<ObjectType, ValueType extends keyof ObjectType = keyof ObjectType> = ObjectType[ValueType];
{
  const value: ValueOf<{a: 1; b: 2; c: 3}> = 3;
  const value1: ValueOf<{a: 1; b: 2; c: 3}> = 1;
  const value2: ValueOf<{a: 1; b: 2; c: 3}> = 2;

  const a0: 1 | 2 | 3 = value;
  const a1: 4 = value;
  //~^ ERROR: Type '3' is not assignable to type '4'.

  const valueRestricted: ValueOf<{a: 1; b: 2; c: 3}, 'a'> = 1;
  const valueRestricted1: ValueOf<{a: 1; b: 2; c: 3}, 'b'> = 2;
  const valueRestricted2: ValueOf<{a: 1; b: 2; c: 3}, 'c'> = 3;

  const b0: 1 = valueRestricted
  const b1: 2 = valueRestricted;
  //~^ ERROR: Type '1' is not assignable to type '2'.
  const b2: 3 = valueRestricted;
  //~^ ERROR: Type '1' is not assignable to type '3'.
}

// ========= WritableKeysOf =========
type WritableKeysOf<T> = NonNullable<{
	[P in keyof T]: 
    IsEqual<
      {[Q in P]: T[P]}, 
      {readonly [Q in P]: T[P]}
    > extends false ? P : never
}[keyof T]>;

{
  type TestType1 = {
    readonly a: string;
    b: boolean;
  };
  
  type TestType2 = {
    a: string;
    b: boolean;
  };
  
  type TestType3 = {
    readonly a: string;
    readonly b: boolean;
  };

  type WritableKeysOf1 = WritableKeysOf<TestType1>;
  type WritableKeysOf2 = WritableKeysOf<TestType2>;
  type WritableKeysOf3 = WritableKeysOf<TestType3>;
  
  // const test1: WritableKeysOf1 = 'b';
  // const test2: WritableKeysOf2 = 'b';
  // const test3: WritableKeysOf2 = 'a';
  // const test4: WritableKeysOf3 = n();
  
  // const a0: 'b' = test1;
  // const a1: 'a' | 'b' = test2;
  // const a2: never = test4;
}