// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/specializedSignatureInInterface.ts`, Apache-2.0 License

interface A {
  (key:string):void;
}

interface B extends A {
  (key:'foo'):string;
  (key:'bar'):string;
}