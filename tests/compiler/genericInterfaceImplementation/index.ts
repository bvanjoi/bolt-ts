// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericInterfaceImplementation.ts`, Apache-2.0 License

interface IOption<A> {
  get(): A;

  flatten<B>(): IOption<B>;
}

class None<T> implements IOption<T>{
  get(): T {
      throw null;
  }

  flatten<U>() : IOption<U> {
      return new None<U>();
  }
}
