// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/testTypings.ts`, Apache-2.0 License

interface IComparable<T> {
  compareTo(other: T);
}

declare function sort<U extends IComparable<U>>(items: U[]): U[];


sort([])
sort([{ compareTo() {} }])
sort([{ }])
//~^ ERROR: Property 'compareTo' is missing.

