//@compiler-options: strict
//@compiler-options: exactOptionalPropertyTypes

type O = {
  b?: boolean;	
}
type B<T extends boolean> = T;
type A<Options extends Required<O>> = B<Options['b']>;

{
  type C<TArray> = TArray extends [(infer First)?] ? [First] : never;
  const c: C<[x?: string]> = [];
  //~^ ERROR: Type '[]' is not assignable to type '[string]'.
}

{
  type IsExactOptionalPropertyTypesEnabled = [(string | undefined)?] extends [string?]
    ? false
    : true;

  const a: IsExactOptionalPropertyTypesEnabled = true;
}

{
    type ArrayTail = ['a', undefined, 'c'?] extends [unknown?, ...infer Tail]
        ? Tail
        : [];
    function f(arr: ArrayTail) { }
    const a: [undefined, 'c'?] = [undefined, 'c'];
    f(a);
}

{
  type E = false & boolean;
  const a: E = true;
  //~^ ERROR: Type 'boolean' is not assignable to type 'false'.
  const b: E = false;
}

{
  type OriginIsIntersection<T> = 1 extends (T & (2 | 3)) ? never : 0;
  const a: OriginIsIntersection<1> = 1;
  //~^ ERROR: Type '1' is not assignable to type '0'.
}

{
  type D = [(42 | undefined)?] extends [(infer _)?] ? boolean: string;
  const d0: D = false;
  const d1: D = true;
}

{
  type H<D> = {
    [KeyType in keyof D]: never;
  };
  type P<T> =
    { [KeyType in keyof T]: never } extends infer U
    ? H<
        {

            [KeyType in keyof ({
                [KeyType in keyof ({ 
									[KeyType in keyof T]: P<T> 
								}) as number
								]: never;
            })]: never;
        }
        &
        U
    >
    : never;
  type A = P<{
    [Key in string]: number;
  }>;
}

{
  // deferred reference type
  type D<T> =  T extends Array<infer ItemType>
      ? ReadonlyArray<D<ItemType>>
      : never
  const a0 = [] as JArray;
  type JArray = JArray[];
  const a1: D<JArray> = a0;
}

{
  type IsAny<T> = 0 extends 1 & NoInfer<T> ? true : false;
  type OnlyAny<T extends IsAny<T> extends true ? any : never> = T;
}

{
  type S<A extends unknown[]> = A extends unknown
      ? (
          keyof A & `${number}` extends never ? [[]] : never
        ) extends infer Result 
        ? Result 
        : never
      : never;
  type E<T extends unknown[]> = S<T>[0][number];
}

{
  type T0<A> = A extends unknown
	? {
			[Key in keyof (0 extends 1 & A ? false : [])]: never
		}
	: never;
  type T1<B extends unknown> = T2<T0<B>>;
  type T2<C extends unknown[]> = false;
}

{
  interface C<T extends {
    object: object;
  }> {}

  type RequiredDeep<T> = T extends C<infer ItemType> ? never : never;
}

{
  function f<T>(a: T): Array<T> {return [a]}
  const a0: Array<42> = f(42);
  const a1: Array<number> = f(42);
  const a2: Array<true> = f(true);
  const a3: Array<boolean> = f(true);
}

{
  type T<A, B> = A extends Record<string, unknown>
			? never
			: A extends unknown[] 
				? B 
				: never
  function f<C, D>(c: C, d: D): T<C, D> {
    throw Error('not implemented');
  }
  const a5: Array<true> = f([], [true]);
}

{
  type T<A> = A extends unknown[] ? A : never;
  function f<B>(a: string[], b: B): T<B> {
    throw Error('not implemented');
  }
  class C<D> {}
  const a6: never = f([], new C());
}

{
	type MergeDeep2<Destination extends Record<PropertyKey, unknown>, Source extends Record<PropertyKey, unknown>> = {
		[Key in keyof Source]: (
			undefined extends Source[Key]
			? (undefined extends Destination[Key] ? undefined : never)
			: never
		);
	}
	function f(a: MergeDeep2<{ foo: {} | undefined }, { foo: { bar: number } | undefined }>) {
		const b: {
			foo: {} | undefined;
		} = a;
	}
}

{
	type G = {
		[index: number]: string;
	};
  type PG = Partial<G>;
	type U = PG extends Partial<infer B>
		? Partial<B> extends PG ? B : PG 
		: never;
	function f(a: U) {
		const b: G = a;
	}

  type U2<A> = A extends Partial<infer B>
		? Partial<B> extends A ? B :A 
		: never;
  type PG2 = U2<string[]>;
  type PG3 = U2<Partial<[string, number, boolean]>>;
}

{
  const emptyObjectSymbol: unique symbol = Symbol();
  type EmptyObject = {[emptyObjectSymbol]?: never};
  const foo: EmptyObject = [];
  //~^ ERROR: Type 'never[]' is not assignable to type 'EmptyObject'.
  type IsEmptyObject = {key: string} extends EmptyObject ? true : false;
  const a0: IsEmptyObject = false;
  const a1: ['a'] extends [EmptyObject] ? true : false = false;
}

{
  type T<A extends unknown[]> =  {
	  [Key in keyof A as Key & (`${number}` | never)]: never;
  };
  function f(a: {0?: never}) {
	  const b: T<[number?]> = a;
  }
}

{
  class G {
	  x?: string;
  }

  type T = G extends {[P in "x"]: G["x"];}
				? false
				: true

  const x: T = true;
}

{
  type A = (a: number, ...arguments_: boolean[]) => null;
  type T = A extends (...arguments_: infer Arguments) => unknown
		? true
		: false;

  function f(b: T) {
    const test1: true = b;
  }
}

{
  type P = {1: boolean};
  type A = [number, string, ...boolean[]];
  type T<TArray> =
      {
        [K in keyof TArray]: K extends `${infer NumberK extends number}` ? (NumberK extends keyof P ? P[NumberK] : TArray[K]) : never
      }

  type D = (...arguments_: T<A>) => null;

  function f(b: D) {
    b(1, true)
  }
}

{
  const s = Symbol('');
  type IsPrimitive = [typeof s] extends [symbol] ? true : false;
  const a: IsPrimitive = true;
}

{
  type AbstractConstructor = abstract new() => object;
  function withBar(ctor: AbstractConstructor) {}
  abstract class Bar {}
  withBar(Bar);
}

{
  type AbstractConstructor = abstract new(...arguments_: any[]) => object;
  function withBar<T extends AbstractConstructor>(Ctor: T) {
	  abstract class ExtendedBar extends Ctor {}
	  return ExtendedBar;
  }
  abstract class Bar {
	  abstract barMethod(): void;
  }

  class WrongConcreteExtendedBar extends withBar(Bar) {}
  //~^ ERROR: Non-abstract class 'WrongConcreteExtendedBar' does not implement inherited abstract member 'barMethod' from class 'ExtendedBar<typeof Bar> & Bar'.
}

{
  class C {
    constructor() {}
  }

  interface I<T0>  {
    new(): T0;
  }

  function f<T1 extends C>(HousingType: I<T1>) {
    class Building extends HousingType {}
    const residence: T1 = new Building();
  }
}

{
  interface A {
    f(): this
  }
  interface B {
    f(): boolean
  }
  const a0: A extends B ? true : false = true;
  //~^ ERROR: Type 'boolean' is not assignable to type 'false'
}

{
  type T1<A, B extends keyof A> = A extends Record<B, A[B]> ? 'a' : 'b';
  type C = [string];
  const a0: T1<C, keyof C> = 'a';
}

{
  type N0<P> = P extends '.a' ? Array<N0<'a'>> : string;
  type N1<F> = (
    F extends unknown ? (f: F) => void : boolean 
  ) extends ((g: infer G) => void)
    ? G & F
    : number;
  type N2<Type> = {[TypeKey in keyof Type]: Type}
  type T0 = N0<'.a'>;
  type T1 = N1<T0>;
  type T2 = N2<T1>;
  function f(a: T2) {
    const b: string[][] = a;
  }
}

{
  type A<B, C> = B extends C ? never : B;
  type D<E> = {
	  [G in keyof E as A<G, 'length'>]: E[G];
  }
  type F = D<[string, string, string]> & {length: 3};
  const a0: F = ['a', 'b', 'c'];

  const a1: {0: string} = ['a'];
  a1[1];
  //~^ ERROR: Element implicitly has an 'any' type because expression of type '1' can't be used to index type '{ 0: string; }'.
}