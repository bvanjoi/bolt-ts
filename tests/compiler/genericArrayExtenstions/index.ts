// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericArrayExtenstions.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export declare class ObservableArray<T> implements Array<T> { // MS.Entertainment.ObservableArray
  //~^ ERROR: Type 'ObservableArray<T, ObservableArray>' is missing the following properties from type 'T[]': length, pop, and 26 more.
concat<U extends T[]>(...items: U[]): T[];
concat(...items: T[]): T[];
}
