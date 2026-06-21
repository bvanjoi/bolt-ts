class A {
  #privateField;
  constructor(arg, exposedField) {
    ({
          key: this.#privateField      
    } = arg);
    this.exposedField = exposedField
    }
  log() {
    console.log(this.#privateField);
    console.log(this.exposedField);
  }
}
class B {
  #privateField;
  constructor(arg, exposedField) {
    'prologue';
    ({
          key: this.#privateField      
    } = arg);
    this.exposedField = exposedField
    }
  log() {
    console.log(this.#privateField);
    console.log(this.exposedField);
  }
}