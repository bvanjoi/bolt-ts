// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveBaseConstructorCreation1.ts`, Apache-2.0 License

// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveBaseConstructorCreation1.ts`, Apache-2.0 License

class C1 {
  public func(param: C2): any { }
}
class C2 extends C1 { }
var x = new C2(); // Valid
  
x.func(new C1()); 
x.func(new C2()); 