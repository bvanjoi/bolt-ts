// From `github.com/sindresorhus/type-fest`, MIT License

function n(): never {
  throw new Error();
}

type ArrayElement<T> = T extends readonly unknown[] ? T[0] : never;
type BaseKeyFilter<Type, Key extends keyof Type> = Key extends symbol
	? never
	: Type[Key] extends symbol
		? never
		: Type[Key] extends Record<string, unknown>
			? Key
			: [(...arguments_: any[]) => any] extends [Type[Key]]
				? never
				: Key;
type BuiltIns = Primitive | void | Date | RegExp;
type FilterDefinedKeys<T extends object> = Exclude<
  {
    [Key in keyof T]: IsAny<T[Key]> extends true
      ? Key
      : undefined extends T[Key]
        ? never
        : T[Key] extends undefined
          ? never
          : BaseKeyFilter<T, Key>;
  }[keyof T],
  undefined
>;
type FilterOptionalKeys<T extends object> = Exclude<
  {
    [Key in keyof T]: IsAny<T[Key]> extends true
      ? never
      : undefined extends T[Key]
        ? T[Key] extends undefined
          ? never
          : BaseKeyFilter<T, Key>
        : never;
  }[keyof T],
  undefined
>;
type FirstArrayElement<TArray extends UnknownArrayOrTuple> = TArray extends readonly [infer THead, ...unknown[]]
	? THead
	: never;
type IsBothExtends<BaseType, FirstType, SecondType> = FirstType extends BaseType
	? SecondType extends BaseType
		? true
		: false
	: false;
type IsLowerCase<T extends string> = T extends Lowercase<T> ? true : false;
type IsUpperCase<T extends string> = T extends Uppercase<T> ? true : false
type LiteralKeyOf<T> = keyof {[K in keyof T as IsLiteral<K> extends true ? K : never]-?: never};
type NegativeInfinity = -1e999;
type Numeric = number | bigint;
type NumericString = '0123456789';
type NonRecursiveType = BuiltIns | Function | (new (...arguments_: any[]) => unknown);
type PositiveInfinity = 1e999;
type RequireNone<KeysType extends PropertyKey> = Partial<Record<KeysType, never>>;
type StringDigit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
type ToString<T> = T extends string | number ? `${T}` : never;
type UnknownArrayOrTuple = readonly [...unknown[]];
type UpperCaseCharacters = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z';
type Whitespace =
	| '\u{9}' // '\t'
	| '\u{A}' // '\n'
	| '\u{B}' // '\v'
	| '\u{C}' // '\f'
	| '\u{D}' // '\r'
	| '\u{20}' // ' '
	| '\u{85}'
	| '\u{A0}'
	| '\u{1680}'
	| '\u{2000}'
	| '\u{2001}'
	| '\u{2002}'
	| '\u{2003}'
	| '\u{2004}'
	| '\u{2005}'
	| '\u{2006}'
	| '\u{2007}'
	| '\u{2008}'
	| '\u{2009}'
	| '\u{200A}'
	| '\u{2028}'
	| '\u{2029}'
	| '\u{202F}'
	| '\u{205F}'
	| '\u{3000}'
	| '\u{FEFF}';
type WordSeparators = '-' | '_' | Whitespace;
type Zero = 0 | 0n;

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
  //~^ ERROR: Type 'number' is not assignable to type '0 | 1 | 2'.
  const a5: ValueKeys = 3;
  //~^ ERROR: Type 'number' is not assignable to type '0 | 1 | 2'.

  type TupleKeys = ArrayIndices<['a', 2]>;

  const testTuple: 0 | 1 = 0;
  const b0: TupleKeys = testTuple;

  const b1: TupleKeys = 0;
  const b2: TupleKeys = 1;

  const b3: TupleKeys = -1;
  //~^ ERROR: Type 'number' is not assignable to type '0 | 1'.
  const b4: TupleKeys = 2;
  //~^ ERROR: Type 'number' is not assignable to type '0 | 1'.

	const c0: ArrayIndices<['a', 'b', 'c']> = 0;
	const c1: ArrayIndices<['a', 'b', 'c']> = 1;
	const c2: ArrayIndices<['a', 'b', 'c']> = 2;
	const c3: ArrayIndices<['a', 'b', 'c']> = 3;
	//~^ ERROR: Type 'number' is not assignable to type '0 | 1 | 2'.
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

// ========== BuildTuple ==========
type BuildTuple<L extends number, Fill = unknown, T extends readonly unknown[] = []> = number extends L
	? Fill[]
	: L extends T['length']
		? T
		: BuildTuple<L, Fill, [...T, Fill]>;
{
  const a0: BuildTuple<3, null> = [null, null, null];
  const a1: BuildTuple<5, 0> = [0, 0, 0, 0, 0];
  const a2: BuildTuple<0, 0> = [];
  const a3: BuildTuple<2 | 3, 0> = {} as [0, 0] | [0, 0, 0];
  const a4: BuildTuple<number, 0> = {} as Array<0>;
}

// ======= HasMultipleCallSignatures =======
type HasMultipleCallSignatures<T extends (...arguments_: any[]) => unknown> =
	T extends {(...arguments_: infer A): unknown; (...arguments_: infer B): unknown}
		? B extends A
			? A extends B
				? false
				: true
			: true
		: false;
{
	type Overloaded = {
		(foo: number): string;
		(foo: string, bar: number): number;
	};
	
	type Overloaded2 = {
		(foo: number | undefined): string;
		(foo: number): string;
	};
	
	type Namespace = {
		(foo: number): string;
		baz: boolean[];
	};
	
	const a0: true = {} as HasMultipleCallSignatures<Overloaded>;
	const a1: false = {} as HasMultipleCallSignatures<Overloaded2>; // dependent on `strictNullChecks` and `strictFunctionTypes`
	const a2: false = {} as HasMultipleCallSignatures<Namespace>;
}
		
// ======= HasOptionalKeys =======
type HasOptionalKeys<BaseType extends object> = OptionalKeysOf<BaseType> extends never ? false : true;
{
  type TestType1 = {
    a: string;
    b?: boolean;
  };
  
  type TestType2 = {
    a?: string;
    b?: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type HasOptionalKeys1 = HasOptionalKeys<TestType1>;
  type HasOptionalKeys2 = HasOptionalKeys<TestType2>;
  type HasOptionalKeys3 = HasOptionalKeys<TestType3>;
  
  const test1: HasOptionalKeys1 = true;
  const test2: HasOptionalKeys2 = true;
  const test3: HasOptionalKeys3 = false;
  
  const a0: true = test1;
  const a1: true = test2;
  const a2: false = test3;
}

// ======= HasReadonlyKeys =======
type HasReadonlyKeys<BaseType extends object> = ReadonlyKeysOf<BaseType> extends never ? false : true;
{
  type TestType1 = {
    a: string;
    readonly b: boolean;
  };
  
  type TestType2 = {
    readonly a: string;
    readonly b: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type HasReadonlyKeys1 = HasReadonlyKeys<TestType1>;
  type HasReadonlyKeys2 = HasReadonlyKeys<TestType2>;
  type HasReadonlyKeys3 = HasReadonlyKeys<TestType3>;
  
  const test1: HasReadonlyKeys1 = true;
  const test2: HasReadonlyKeys2 = true;
  const test3: HasReadonlyKeys3 = false;
  
  const a0: true = test1;
  const a1: true = test2;
  const a2: false = test3;
}

// ======= HasRequiredKeys =======
type HasRequiredKeys<BaseType extends object> = RequiredKeysOf<BaseType> extends never ? false : true;
{
  type TestType1 = {
    a: string;
    b?: boolean;
  };
  
  type TestType2 = {
    a?: string;
    b?: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type HasRequiredKeys1 = HasRequiredKeys<TestType1>;
  type HasRequiredKeys2 = HasRequiredKeys<TestType2>;
  type HasRequiredKeys3 = HasRequiredKeys<TestType3>;
  
  const test1: HasRequiredKeys1 = true;
  const test2: HasRequiredKeys2 = false;
  const test3: HasRequiredKeys3 = true;
  
  const a0: true = test1;
  const a1: false = test2;
  const a2: true = test3;
}

// ======= HasWritableKeys =======
type HasWritableKeys<BaseType extends object> = WritableKeysOf<BaseType> extends never ? false : true;
{
  type TestType1 = {
    a: string;
    readonly b: boolean;
  };
  
  type TestType2 = {
    readonly a: string;
    readonly b: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type HasWritableKeys1 = HasWritableKeys<TestType1>;
  type HasWritableKeys2 = HasWritableKeys<TestType2>;
  type HasWritableKeys3 = HasWritableKeys<TestType3>;
  
  const test1: HasWritableKeys1 = true;
  const test2: HasWritableKeys2 = false;
  const test3: HasWritableKeys3 = true;
  
  const a0: true = test1;
  const a1: false = test2;
  const a2: true = test3;
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


// ======= IfArrayReadonly =======
type IfArrayReadonly<T extends UnknownArray, TypeIfArrayReadonly = true, TypeIfNotArrayReadonly = false> =
	IsArrayReadonly<T> extends infer Result
		? Result extends true ? TypeIfArrayReadonly : TypeIfNotArrayReadonly
		: never; // Should never happen
{
  // Non-readonly arrays
  const a0: IfArrayReadonly<[]> = false;
  const a1: IfArrayReadonly<number[], string, number> = {} as number;
  const a2: IfArrayReadonly<[string?, number?], string> = false;
  const a3: IfArrayReadonly<[string, number, ...string[]], false, true> = true;

  // Readonly arrays
  const b0: IfArrayReadonly<readonly []> = true;
  const b1: IfArrayReadonly<readonly number[], string, number> = {} as string;
  const b2: IfArrayReadonly<readonly [string?, number?], string> = {} as string;
  const b3: IfArrayReadonly<readonly [string, number, ...string[]], false, true> = false;

  // Union
  const c0: IfArrayReadonly<[] | [string, number]> = false;
  const c1: IfArrayReadonly<[] | [string, number], string, number> = {} as number;
  const c2: IfArrayReadonly<readonly [] | readonly [string, number]> = true;
  const c3: IfArrayReadonly<readonly [] | readonly [string, number], string, number> = {} as string;

  // Returns union of `TypeIfArrayReadonly` and `TypeIfNotArrayReadonly` when `T` is a union of readonly and non-readonly arrays.
  const d0: IfArrayReadonly<[] | readonly []> = {} as boolean;
  const d1: IfArrayReadonly<[string, number] | readonly [string, number, ...string[]], string, number> = {} as string | number;
  const d2: IfArrayReadonly<[string, number] | readonly [string, number, ...string[]], string> = {} as string | false;

  // Returns union of `TypeIfArrayReadonly` and `TypeIfNotArrayReadonly` when `T` is `any`.
  const e0: IfArrayReadonly<any> = {} as boolean;
  const e1: IfArrayReadonly<any, string, number> = {} as string | number;
  const e2: IfArrayReadonly<any, string> = {} as string | false;

  // Returns `TypeIfNotArrayReadonly` when `T` is `never`.
  const f0: IfArrayReadonly<never> = false;
  const f1: IfArrayReadonly<never, string, number> = {} as number;
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

// ========= IsArrayReadonly =========
type IsArrayReadonly<T extends UnknownArray> = IfNever<T, false, T extends unknown[] ? false : true>;
{
  // Non-readonly arrays
  const a0: IsArrayReadonly<[]> = false;
  const a1: IsArrayReadonly<number[]> = false;
  const a2: IsArrayReadonly<[string, number?, ...string[]]> = false;
  const a3: IsArrayReadonly<[x: number, y: number, z?: number]> = false;
  const a4: IsArrayReadonly<[...string[], number, string]> = false;

  // Readonly arrays
  const b0: IsArrayReadonly<readonly []> = true;
  const b1: IsArrayReadonly<readonly number[]> = true;
  const b2: IsArrayReadonly<readonly [string, number?, ...string[]]> = true;
  const b3: IsArrayReadonly<readonly [x: number, y: number, z?: number]> = true;
  const b4: IsArrayReadonly<readonly [...string[], number, string]> = true;

  // Union
  const c0: IsArrayReadonly<[] | readonly []> = {} as boolean;
  const c1: IsArrayReadonly<[string, number] | readonly [string, number, ...string[]]> = {} as boolean;
  const c2: IsArrayReadonly<[] | [string, number]> = false;
  const c3: IsArrayReadonly<readonly [] | readonly [string, number]> = true;

  // Boundary types
  const d0: IsArrayReadonly<any> = {} as boolean;
  const d1: IsArrayReadonly<never> = false;
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

// =========== IsFloat ===========
type IsFloat<T> =
T extends number
	? `${T}` extends `${infer _Sign extends '' | '-'}${number}.${infer Decimal extends number}`
		? Decimal extends Zero
			? false
			: true
		: false
	: false;

{
	const a0: false = {} as IsFloat<0>;
	const a1: false = {} as IsFloat<1>;
	const a2: false = {} as IsFloat<1.0>;
	const a3: true = {} as IsFloat<1.5>;
	const a4: false = {} as IsFloat<-1>;
	const a5: false = {} as IsFloat<number>;
	const a6: false = {} as IsFloat<0o10>;
	// const a7: false = {} as IsFloat<1n>;
	// const a8: false = {} as IsFloat<0n>;
	const a9: false = {} as IsFloat<0b10>;
	// const a10: false = {} as IsFloat<0x10>;
	const a11: false = {} as IsFloat<1e+100>;
	// const a12: false = {} as IsFloat<PositiveInfinity>;
	// const a13: false = {} as IsFloat<typeof Number.POSITIVE_INFINITY>;
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

// =========== IsNotFalse ===========
type IsNotFalse<T extends boolean> = [T] extends [false] ? false : true;
{
  const a0: IsNotFalse<true> = false;
  //~^ ERROR: Type 'false' is not assignable to type 'true'.
  const a1: IsNotFalse<boolean> = false;
  //~^ ERROR: Type 'false' is not assignable to type 'true'.
  const a2: IsNotFalse<true | false> = false;
  //~^ ERROR: Type 'false' is not assignable to type 'true'.
  const a3: IsNotFalse<true | false | false | false> = false;
  //~^ ERROR: Type 'false' is not assignable to type 'true'.
  const a4: IsNotFalse<false> = true;
  //~^ ERROR: Type 'true' is not assignable to type 'false'.
  const a5: IsNotFalse<false | false> = true;
  //~^ ERROR: Type 'true' is not assignable to type 'false'.
  const a6: IsNotFalse<false | false | false | false> = true;
  //~^ ERROR: Type 'true' is not assignable to type 'false'.
}

// ============= isNumberLike =============
type IsNumberLike<N> =
	N extends number ? true
		:	N extends `${number}`
			? true
			: N extends `${number}.${number}`
				? true
				: false;
{
  const a0: IsNumberLike<'1'> = true;
  const a1: IsNumberLike<1> = true;
  const a2: IsNumberLike<'-1.1'> = true;
  const a3: IsNumberLike< -1.1> = true;
  const a4: IsNumberLike<'foo'> = false;
}

// ============= isNull =============
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

// ============= IsNumeric =============
type IsNumeric<T extends string> = T extends `${number}`
	? Trim<T> extends T
		? true
		: false
	: false;
{
  const a0: IsNumeric<''> = false;
  const a1: IsNumeric<'0'> = true;
  const a2: IsNumeric<'1'> = true;
  const a3: IsNumeric<'-1'> = true;
  const a4: IsNumeric<'123'> = true;
  const a5: IsNumeric<'1e2'> = true;
  const a6: IsNumeric<'1.23'> = true;
  const a7: IsNumeric<'123.456'> = true;
  const a8: IsNumeric<'1.23e4'> = true;
  const a9: IsNumeric<'1.23e-4'> = true;
  const a10: IsNumeric<' '> = false;
  const a11: IsNumeric<'\n'> = false;
  const a12: IsNumeric<'\u{9}'> = false;
  const a13: IsNumeric<' 1.2'> = false;
  const a14: IsNumeric<'1 2'> = false;
  const a15: IsNumeric<'1_200'> = false;
  const a16: IsNumeric<' 1 '> = false;
}

// =========== IsPrimitive ===========
type IsPrimitive<T> = [T] extends [Primitive] ? true : false;
{
  let a: IsPrimitive<'string'> = true;
  let b: IsPrimitive<string> = true;
  let c: IsPrimitive<Object> = false;
}

// =========== IsUnion ===========
type IsUnion<T> = InternalIsUnion<T>;

/**
The actual implementation of `IsUnion`.
*/
type InternalIsUnion<T, U = T> =
(
	// @link https://ghaiklor.github.io/type-challenges-solutions/en/medium-isunion.html
	IsNever<T> extends true
		? false
		: T extends any
			? [U] extends [T]
				? false
				: true
			: never
) extends infer Result
	// In some cases `Result` will return `false | true` which is `boolean`,
	// that means `T` has at least two types and it's a union type,
	// so we will return `true` instead of `boolean`.
	? boolean extends Result ? true
		: Result
	: never; // Should never happen
{
  const a0: IsUnion<1> = false;
  const a1: IsUnion<true> = false;
  const a2: IsUnion<'foo'> = false;
  const a3: IsUnion<[]> = false;
  const a4: IsUnion<{}> = false;
  const a5: IsUnion<1 & {}> = false;
  const a6: IsUnion<never> = false;
  const a7: IsUnion<unknown> = false;
  const a8: IsUnion<any> = false;

  const b0: IsUnion<1 | 2> = true;
  const b1: IsUnion<'foo' | 'bar'> = true;
  const b2: IsUnion<'foo' | 'bar' | 1> = true;
  const b3: IsUnion<'foo' | 1> = true;
  const b4: IsUnion<[] | {}> = true;
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


// ========== IsWhitespace ==========
type IsWhitespace<T extends string> = T extends Whitespace
	? true
	: T extends `${Whitespace}${infer Rest}`
		? IsWhitespace<Rest>
		: false;
{
  const a0: IsWhitespace<''> = false;
  const a1: IsWhitespace<' '> = true;
  const a2: IsWhitespace<'\n'> = true;
  const a3: IsWhitespace<'\u{9}'> = true;
  const a4: IsWhitespace<'a'> = false;
  const a5: IsWhitespace<'a '> = false;
  const a6: IsWhitespace<'   '> = true;
  const a7: IsWhitespace<' \t '> = true;
}

// ========= NonEmptyTuple =========
type NonEmptyTuple<T = unknown> = readonly [T, ...T[]];
{
  const sum: (...numbers: NonEmptyTuple<number>) => number = () => 42;
  const a0: number = sum(1, 2, 3);
  const a1: number = sum(1);
  sum();
  //~^ ERROR: Expected 1 arguments, but got 0.
}

// ============== Not ==============
type Not<A extends boolean> = A extends true
	? false
	: A extends false
		? true
		: never;
{
	const a0: Not<true> = false;
	const a1: Not<false> = true;
	// FIXME
	const a2: Not<boolean> = null! as boolean;
}

// ========= OptionalKeysOf =========
type OptionalKeysOf<BaseType extends object> = Exclude<{
	[Key in keyof BaseType]: BaseType extends Record<Key, BaseType[Key]> ? never : Key
}[keyof BaseType], undefined>;
{
  type TestType1 = {
    a: string;
    b?: boolean;
  };
  
  type TestType2 = {
    a?: string;
    b?: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type OptionalKeysOf1 = OptionalKeysOf<TestType1>;
  type OptionalKeysOf2 = OptionalKeysOf<TestType2>;
  type OptionalKeysOf3 = OptionalKeysOf<TestType3>;
  
  const test1: OptionalKeysOf1 = 'b';
  const test2: OptionalKeysOf2 = 'a';
  const test3: OptionalKeysOf2 = 'b';
  const test4: OptionalKeysOf3 = n();
  
  const a0: 'b' = test1;
  const a1: 'a' | 'b' = test2;
  const a2: never = test4;
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

// ========= RequiredKeysOf =========
type RequiredKeysOf<BaseType extends object> = Exclude<{
	[Key in keyof BaseType]: BaseType extends Record<Key, BaseType[Key]>
		? Key
		: never
}[keyof BaseType], undefined>;
{
  type TestType1 = {
    a: string;
    b?: boolean;
  };
  
  type TestType2 = {
    a?: string;
    b?: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type RequiredKeysOf1 = RequiredKeysOf<TestType1>;
  type RequiredKeysOf2 = RequiredKeysOf<TestType2>;
  type RequiredKeysOf3 = RequiredKeysOf<TestType3>;
  
  const test1: RequiredKeysOf1 = 'a';
  const test2: RequiredKeysOf2 = n();
  const test3: RequiredKeysOf3 = 'a';
  const test4: RequiredKeysOf3 = 'b';
  
  const a0: 'a' = test1;
  const a1: never = test2;
  const a2: 'a' | 'b' = test3;
}

// === PositiveNumericCharacterGt ===
type PositiveNumericCharacterGt<A extends string, B extends string> = NumericString extends `${infer HeadA}${A}${infer TailA}`
	? NumericString extends `${infer HeadB}${B}${infer TailB}`
		? HeadA extends `${HeadB}${infer _}${infer __}`
			? true
			: false
		: never
	: never;
{
  let a0: PositiveNumericCharacterGt<'5', '1'> = true;
  let a1: PositiveNumericCharacterGt<'5', '5'> = false;
  let a2: PositiveNumericCharacterGt<'1', '5'> = false;
  let a3: PositiveNumericCharacterGt<'30', '2'> = n();
}

// === PositiveNumericStringGt ===
type PositiveNumericStringGt<A extends string, B extends string> = A extends B
	? false
	: [BuildTuple<StringLength<A>, 0>, BuildTuple<StringLength<B>, 0>] extends infer R extends [readonly unknown[], readonly unknown[]]
		? R[0] extends [...R[1], ...infer Remain extends readonly unknown[]]
			? 0 extends Remain['length']
				? SameLengthPositiveNumericStringGt<A, B>
				: true
			: false
		: never;
{
  let a0: PositiveNumericStringGt<'500', '1'> = false;
  //~^ ERROR: Type 'false' is not assignable to type 'true'.
  let a1: PositiveNumericStringGt<'1', '1'> = true;
  //~^ ERROR: Type 'true' is not assignable to type 'false'.
  let a2: PositiveNumericStringGt<'1', '500'> = true;
  //~^ ERROR: Type 'true' is not assignable to type 'false'.
}

// =========== Primitive ===========
type Primitive =
	| null
	| undefined
	| string
	| number
	| boolean
	| symbol
	| bigint;

// ========== Promisable ==========
type Promisable<T> = T | PromiseLike<T>;
{
  const promisable: Promisable<string> = '';
  const a0: PromiseLike<string> | string = promisable;
  const a1: Promisable<string> = Promise.resolve(42);
}

// ======== ReadonlyKeysOf ========
type ReadonlyKeysOf<T> = NonNullable<{
	[P in keyof T]: IsEqual<{[Q in P]: T[P]}, {readonly [Q in P]: T[P]}> extends true ? P : never
}[keyof T]>;
{
  type TestType1 = {
    a: string;
    readonly b: boolean;
  };
  
  type TestType2 = {
    readonly a: string;
    readonly b: boolean;
  };
  
  type TestType3 = {
    a: string;
    b: boolean;
  };
  
  type ReadonlyKeysOf1 = ReadonlyKeysOf<TestType1>;
  type ReadonlyKeysOf2 = ReadonlyKeysOf<TestType2>;
  type ReadonlyKeysOf3 = ReadonlyKeysOf<TestType3>;
  
  const test1: ReadonlyKeysOf1 = 'b';
  const test2: ReadonlyKeysOf2 = 'a';
  const test3: ReadonlyKeysOf2 = 'b';
  const test4: ReadonlyKeysOf3 = n();
  
  const a0: 'b' = test1;
  const a1: 'a' = test2;
  const a3: 'b' = test3;
  const a2: never = test4;
}

// ========= ReadonlyTuple =========
type BuildTupleHelper<Element, Length extends number, Rest extends Element[]> =
	Rest['length'] extends Length ?
		readonly [...Rest] : // Terminate with readonly array (aka tuple)
		BuildTupleHelper<Element, Length, [Element, ...Rest]>;

type ReadonlyTuple<Element, Length extends number> =
    number extends Length
      // Because `Length extends number` and `number extends Length`, then `Length` is not a specific finite number.
      ? readonly Element[] // It's not fixed length.
      : BuildTupleHelper<Element, Length, []>; // Otherwise it is a fixed length tuple.
{
  type TupleOfThreeStrings = ReadonlyTuple<string, 3>;

  const test: TupleOfThreeStrings = ['a', 'b', 'c'];

  const b0: TupleOfThreeStrings = ['a', 'b', 123];
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  const b1: TupleOfThreeStrings = ['a'];
  //~^ ERROR: Type '[string]' is not assignable to type '[string, string, string]'.
  const b2: TupleOfThreeStrings = ['a', 'b'];
  //~^ ERROR: Type '[string, string]' is not assignable to type '[string, string, string]'.
  const b3: TupleOfThreeStrings = ['a', 'b', 'c', 'd'];
  //~^ ERROR: Type '[string, string, string, string]' is not assignable to type '[string, string, string]'.

  const _a: unknown = test.push;
  //~^ ERROR: Property 'push' does not exist on type '[string, string, string]'.
  test[2] = 'a';
  //~^ ERROR: Cannot assign to '2' because it is a read-only property.
}

// = SameLengthPositiveNumericStringGt =
type SameLengthPositiveNumericStringGt<A extends string, B extends string> = A extends `${infer FirstA}${infer RestA}`
	? B extends `${infer FirstB}${infer RestB}`
		? FirstA extends FirstB
			? SameLengthPositiveNumericStringGt<RestA, RestB>
			: PositiveNumericCharacterGt<FirstA, FirstB>
		: never
	: false;
{
	let a0: SameLengthPositiveNumericStringGt<'50', '10'> = true;
	let a1: SameLengthPositiveNumericStringGt<'50', '50'> = false;
}

// ========= SetArrayAccess =========
type SetArrayAccess<T extends UnknownArray, IsReadonly extends boolean> =
T extends readonly [...infer U] ?
	IsReadonly extends true
		? readonly [...U]
		: [...U]
	: T;
{
  let a0: SetArrayAccess<string[], true> = [];
  a0.push('42');
  //~^ ERROR: Property 'push' does not exist on type 'unknown[]'.
  let a1: SetArrayAccess<string[], false> = [];
  a1.push('42');
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

// ============ StartsWith ============
type StartsWith<S extends string, SearchString extends string> = string extends S | SearchString
	? never
	: S extends `${SearchString}${infer T}`
		? true
		: false;
{
  let a0: StartsWith<'abcde', 'abc'> = true;
  let a1: StartsWith<'abcde', 'bc'> = false;
  let a2: StartsWith<string, 'bc'> = n();
  let a3: StartsWith<'abcde', string> = n();
}

// ========= StaticPartOfArray =========
type StaticPartOfArray<T extends UnknownArray, Result extends UnknownArray = []> =
	T extends unknown
		? number extends T['length'] ?
			T extends readonly [infer U, ...infer V]
				? StaticPartOfArray<V, [...Result, U]>
				: Result
			: T
		: never;
{
  type A = [string, number, boolean, ...string[]];
  type B = StaticPartOfArray<A>;
  let b: B = [];
  //~^ ERROR: Type '[]' is not assignable to type '[string, number, boolean]'.
}

// ============ Stringified ============
type Stringified<ObjectType> = {[KeyType in keyof ObjectType]: string};
{
  const stringified: Stringified<{a: number; b: string}> = {a: 'a', b: 'b'};
  const a0: {a: string; b: string} = stringified;

  type Car = {
    model: string;
    speed: number;
  };
  const b0: Stringified<Car> = {model: 'Foo', speed: 101};
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  const b1: Stringified<Car> = {model: 'Foo', speed: '101'};
}

// ========== StringLength ==========
type StringLength<S extends string> = string extends S
	? never
	: StringToArray<S>['length'];
{
  let a0: StringLength<'abcde'> = 4;
  //~^ ERROR: Type '4' is not assignable to type '5'.
  let a1: StringLength<string> = n();
}

// ========== StringToArray ==========
type StringToArray<S extends string, Result extends string[] = []> = string extends S
	? never
	: S extends `${infer F}${infer R}`
		? StringToArray<R, [...Result, F]>
		: Result;
{
  let a0: StringToArray<'abcde'> = ['a', 'b', 'c', 'd'];
  //~^ ERROR: Type '["a", "b", "c", "d"]' is not assignable to type '["a", "b", "c", "d", "e"]'.
  let a1: StringToArray<string> = n();
}

// ========== StringToNumber ==========
type StringToNumber<S extends string> = S extends `${infer N extends number}`
	? N
	: S extends 'Infinity'
		? PositiveInfinity
		: S extends '-Infinity'
			? NegativeInfinity
			: never;
{
  let a0: StringToNumber<'1234'> = 42;
  //~^ ERROR: Type '42' is not assignable to type '1234'.
  let a1: StringToNumber<'-1234'> = 42;
  //~^ ERROR: Type '42' is not assignable to type '-1234'.
  let a2: StringToNumber<'1234.56'> = 42;
  //~^ ERROR: Type '42' is not assignable to type '1234.56'.
  let a3: StringToNumber<'-1234.56'> = 42;
  //~^ ERROR: Type '42' is not assignable to type '-1234.56'.
  let a4: StringToNumber<'Infinity'> = 42;
  //~^ ERROR: Type '42' is not assignable to type 'Infinity'.
  let a5: StringToNumber<'-Infinity'> = 42;
  //~^ ERROR: Type '42' is not assignable to type '-Infinity'.
}

// ============ TaggedUnion ============
type TaggedUnion<
	TagKey extends string,
	UnionMembers extends Record<string, Record<string, unknown>>,
> = {
	[Name in keyof UnionMembers]: {[Key in TagKey]: Name} & UnionMembers[Name];
}[keyof UnionMembers];
{
  type Union = TaggedUnion<'tag', {str: {a: string} ; num: {b: number}}>;
  const first = {
    tag: 'str' as const,
    a: 'some-string',
  };

  const second = {
    tag: 'num' as const,
    b: 1,
  };

  const a0: Union = first;
  const a1: Union = second;

  const fails = {
    tag: 'num' as const,
    b: 'should not be string',
  };

  const failsToo = {
    tag: 'str' as const,
    b: 2,
  };

  const b0: Union = fails;
  //~^ ERROR: Type '{ tag: "num"; b: string; }' is not assignable to type 'mapped type & { a: string; } | mapped type & { b: number; }'.
  const b1: Union = failsToo;
  //~^ ERROR: Type '{ tag: "str"; b: number; }' is not assignable to type 'mapped type & { a: string; } | mapped type & { b: number; }'.
}

// ================ Trim ================
type TrimLeft<V extends string> = V extends `${Whitespace}${infer R}` ? TrimLeft<R> : V;

type TrimRight<V extends string> = V extends `${infer R}${Whitespace}` ? TrimRight<R> : V;

type Trim<V extends string> = TrimLeft<TrimRight<V>>;
{
	function trim<S extends string>(value: S): Trim<S> { return value as Trim<S> }

	const a0: 'foo' = trim(' foo');
	const a1: 'bar' = trim('bar ');
	const a2: 'baz' = trim(' baz ');
	const a3: 'waldo' = trim('  waldo  ');
	const a4: 'fr ed' = trim(' fr ed ');
	const a5: 'foo' = trim(' foo\n');
	const a6: 'foo' = trim(' foo\n\t ');
	const a7: ' foo ' = trim(' foo ');
  //~^ ERROR: Type '"foo"' is not assignable to type '" foo "'.
}

// =========== TupleLength ===========
type TupleLength<T extends UnknownArray> =
	// `extends unknown` is used to convert `T` (if `T` is a union type) to
	// a [distributive conditionaltype](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html#distributive-conditional-types))
	T extends unknown
		? number extends T['length']
			? never // Return never if the given type is an non-flexed-length array like `Array<string>`
			: T['length']
		: never;
{
  const a0: TupleLength<[string, number, boolean]> = 42;
  //~^ ERROR: Type '42' is not assignable to type '3'.
  const a1: TupleLength<string[]> = n();
  const a2: TupleLength<[] | [1, 2, 3] | Array<number>> = 42;
  //~^ ERROR: Type 'number' is not assignable to type '0 | 3'.
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
  //~^ ERROR: Type 'DateConstructor' is missing the following properties from type 'unknown[]': join, forEach, and
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

// ======== VariablePartOfArray ========
type VariablePartOfArray<T extends UnknownArray> =
	T extends unknown
		? T extends readonly [...StaticPartOfArray<T>, ...infer U]
			? U
			: []
		: never;
{
  type A = [string, number, boolean, ...string[]];
  type B = VariablePartOfArray<A>;
  let b: VariablePartOfArray<A> = [42];
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

// ============ ValueOf ============
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
	[P in keyof T]: IsEqual<{[Q in P]: T[P]}, {readonly [Q in P]: T[P]}> extends false ? P : never
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
  
  const test1: WritableKeysOf1 = 'b';
  const test2: WritableKeysOf2 = 'b';
  const test3: WritableKeysOf2 = 'a';
  const test4: WritableKeysOf3 = n();
  
  const a0: 'b' = test1;
  const a1: 'a' | 'b' = test2;
  const a2: never = test4;
}
