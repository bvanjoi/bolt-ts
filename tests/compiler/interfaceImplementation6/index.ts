// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceImplementation6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

interface I1 {
    item:number;
}

class C1 implements I1 {
    public item:number;
    //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
}

class C2 implements I1 {
  //~^ ERROR: Class 'C2' incorrectly implements interface 'I1'.
    private item:number;
    //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
}

class C3 implements I1 {
  //~^ ERROR: Property 'item' is missing
    constructor() {
       var item: number;
    }
}
 
export class Test {
    private pt: I1 = { item: 1 };
}