// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declareClassInterfaceImplementation.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IBuffer {
    [index: number]: number;
}

declare class Buffer implements IBuffer {
  //~^ ERROR: Class 'Buffer' incorrectly implements interface 'IBuffer'.

}
