// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/validUseOfThisInSuper.ts`, Apache-2.0 License

class Base {
  constructor(public b: Base) {
  }
}
class Super extends Base {
  constructor() {
      super((() => this)()); // ok since this is not the case: The constructor declares parameter properties or the containing class declares instance member variables with initializers.
  }
}