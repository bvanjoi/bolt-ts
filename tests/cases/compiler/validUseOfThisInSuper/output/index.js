// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/validUseOfThisInSuper.ts`, Apache-2.0 License
class Base {
  constructor(b) {
    this.b = b}
}
class Super extends Base {
  constructor() {super((() => this)());}
}