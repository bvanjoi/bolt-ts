// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInGenericStaticMembers.ts`, Apache-2.0 License

class A {

  static one<T>(source:T, value: number): T {
      return source;
  }

  static two<T>(source: T): T {
      this.one<T>(source, '42');
      //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
      return this.one<T>(source, 42);
  }
}

class B {

  static one(source: B, value: number): B {
      return source;
  }

  static two(source: B): B {
      this.one(source, '42');
      //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
      return this.one(source, 42);
  }
}


