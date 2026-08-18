// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowPrivateClassField.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: strict
class Example {
  #test;
  constructor(test) {this.#test = test;}
  get test() {
    return this.#test;
  }
}
class Example2 {
  #test;
  constructor(test) {this.#test = test;}
  get test() {
    if (this.#test) {
      return this.#test;
    }
    
    return 0;
  }
}