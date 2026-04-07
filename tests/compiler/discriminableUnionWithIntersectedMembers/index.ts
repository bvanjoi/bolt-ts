// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminableUnionWithIntersectedMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
type X = 
 | { x: 'x', y: number } & { y: number } 
 | { x: 'y', y: number, z?: boolean } & { y: number }

// error
const x: X = 4 as any as { x: 'x' | 'y', y: number };

type Y = 
 | { x: 'x', y: number } 
 | { x: 'y', y: number, z?: boolean }

// no  error
const y: Y = 4 as any as { x: 'x' | 'y', y: number };


{
  interface A {
		[Symbol.toPrimitive]: 'A';
  }
  interface B {
    [Symbol.toPrimitive]: 'B';
  }
  type C = A | B;
  type D<T> = T extends C 
    ? never
    : T extends [infer U, ...infer V]
    ? [...D<V>]
    : never;
}