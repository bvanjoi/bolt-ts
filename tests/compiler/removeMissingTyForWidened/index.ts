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