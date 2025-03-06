// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveConditionalCrash2.ts`, Apache-2.0 License

export type CanBeExpanded<T extends object> = {
  value: T
}

type Expand__<O, N, Depth> =
  N extends Depth ?
      unknown :
      O extends CanBeExpanded<any> ?
          Expand__<O['value'], N, Depth> :
          O

export type UseQueryOptions<T> = Expand__<T, 4, 2>

let a: UseQueryOptions<number> = '42';
//~^ ERROR: Type 'string' is not assignable to type 'number'.
let b: UseQueryOptions<{value: 42}> = '42';
//~^ ERROR: Type 'string' is not assignable to type '42'.
