// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/getterThatThrowsShouldNotNeedReturn.ts`, Apache-2.0 License

class Greeter {
  public get greet(): string {
   throw ''; // should not raise an error
  }
  public greeting(): string {
   throw ''; // should not raise an error
  }
 }
 