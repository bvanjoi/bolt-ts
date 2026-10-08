// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/subclassWithPolymorphicThisIsAssignable.ts`, Apache-2.0 License
//@compiler-options: target=es2015
/* taken from mongoose.Document */
/* our custom model extends the mongoose document */
export class Example {
  constructor() {this.test()// types of increment not compatible??
    ;}
  test() {}
}