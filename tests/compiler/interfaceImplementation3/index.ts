// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceImplementation3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1 {
    iObj:{ };
    iNum:number;
    iAny:any;
    iFn():void;
}

class C4 implements I1 {
  //~^ ERROR: Property 'iAny' is missing
    public iObj:{ };
    //~^ ERROR: Property 'iObj' has no initializer and is not definitely assigned in the constructor.
    public iNum:number;
    //~^ ERROR: Property 'iNum' has no initializer and is not definitely assigned in the constructor.
    public iFn() { }
}


