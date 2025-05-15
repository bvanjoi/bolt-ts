

// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/subclassWithPolymorphicThisIsAssignable.ts`, Apache-2.0 License
/* taken from mongoose.Document *//* our custom model extends the mongoose document */class Example {
  constructor() {// types of increment not compatible??
    this.test();}
  test() {}
}