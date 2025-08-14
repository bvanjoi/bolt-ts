// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyExpandingTypesNonGenericBase.ts`, Apache-2.0 License

class Functionality<V> {
  property: Options<V>;
}

class Base {
}

class A<T> extends Base {
  options: Options<Functionality<T>[]>;
}

interface OptionsBase<T> {
  Options: Options<T>;
}

interface Options<T> extends OptionsBase<T> {
}


function o(type: new () => Base) {
}

o(A);
