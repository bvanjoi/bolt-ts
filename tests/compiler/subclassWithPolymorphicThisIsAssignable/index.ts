// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/subclassWithPolymorphicThisIsAssignable.ts`, Apache-2.0 License

/* taken from mongoose.Document */
interface Document {
  increment(): this;
}

/* our custom model extends the mongoose document */
interface CustomDocument extends Document { }

export class Example<Z extends CustomDocument> {
  constructor() {
      // types of increment not compatible??
      this.test<Z>();
  }

  public test<Z extends Document>() { }
}
