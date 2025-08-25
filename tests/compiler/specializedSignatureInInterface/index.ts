// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/specializedSignatureInInterface.ts`, Apache-2.0 License

interface A {
  (key:string):void;
}

interface B extends A {
  (key:'foo'):string;
  (key:'bar'):string;
}

function f(a: A, b: B) {
  let a0: void = a('foo');
  let b0: string = b('foo');
  let b1: string = b('bar');
}