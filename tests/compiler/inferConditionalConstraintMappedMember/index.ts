// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferConditionalConstraintMappedMember.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

type KeysWithoutStringIndex<T> =
    { [K in keyof T]: string extends K ? never : K } extends { [_ in keyof T]: infer U }
    ? U
    : never

// Only "foo" | "bar" as expected, [string] index signature removed
type test = KeysWithoutStringIndex<{ [index: string]: string; foo: string; bar: 'baz' }>

// KeysWithoutStringIndex<T> will always be a subset of keyof T, but is reported as unassignable
export type RemoveIdxSgn<T> = Pick<T, KeysWithoutStringIndex<T>>
  // ERROR:
  // Type 'KeysWithoutStringIndex<T>' does not satisfy the constraint 'keyof T'.
  //  Type 'unknown' is not assignable to type 'keyof T'.(2344)

type test2 = KeysWithoutStringIndex<{ foo: string; bar: 'baz', [123]: '42' }>

const t0: test = 'bar'; // error, not string index signature
//~^ ERROR: Type 'string' is not assignable to type 'never'.
const t2: test2 = 42;
//~^ ERROR: Type 'number' is not assignable to type '"foo" | "bar" | 123'.