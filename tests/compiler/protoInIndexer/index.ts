// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/protoInIndexer.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class X {
  constructor() {
      this['__proto__'] = null; // used to cause ICE
  }
}

new X()
