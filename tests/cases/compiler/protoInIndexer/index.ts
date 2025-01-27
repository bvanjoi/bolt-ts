// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/protoInIndexer.ts`, Apache-2.0 License

class X {
  constructor() {
      this['__proto__'] = null; // used to cause ICE
  }
}

new X()