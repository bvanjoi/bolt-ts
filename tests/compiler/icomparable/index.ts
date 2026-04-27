// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/icomparable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

  interface IComparable<T> {
      compareTo(other: T);
  }

  declare function sort<U extends IComparable<any>>(items: U[]): U[];

  interface StringComparable extends IComparable<string> {
  }

  var sc: StringComparable[];

    var x = sort(sc);