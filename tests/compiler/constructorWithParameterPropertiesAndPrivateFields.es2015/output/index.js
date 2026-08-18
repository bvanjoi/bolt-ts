// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorWithParameterPropertiesAndPrivateFields.es2015.ts`, Apache-2.0 License
//@compiler-options: target=es2015
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
class C {
  a;
  constructor(arg) {({
          key: this.a      
    } = arg);}
}