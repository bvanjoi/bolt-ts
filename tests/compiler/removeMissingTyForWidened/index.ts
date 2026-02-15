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
  type Subtract3<T> = 1 extends (T & (2 | 3)) ? never : 0;
  const a: Subtract3<1> = 1;
  //~^ ERROR: Type '1' is not assignable to type '0'.
}