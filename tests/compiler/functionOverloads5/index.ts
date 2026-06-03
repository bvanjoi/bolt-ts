// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class baz { 
  public foo();   //~ERROR: Overload signatures must all be public, private or protected.
  private foo(bar?:any){ }
}


const a = 'foo';
class bar { 
  public foo();   //~ERROR: Overload signatures must all be public, private or protected.
  private [a](bar?:any){ }
}