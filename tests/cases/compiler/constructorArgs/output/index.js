
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorArgs.ts`, Apache-2.0 License
class Super {
  constructor(value) {}
}
class Sub extends Super {
  constructor(options) {
    super(options.value);
    this.options = options
    }
}