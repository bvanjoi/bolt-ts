// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/protoInIndexer.ts`, Apache-2.0 License
class X {
  constructor() {this["__proto__"] = null;}
}
// used to cause ICE
new X();