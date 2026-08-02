// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceImplementation8.ts`, Apache-2.0 License

//@compiler-options: target=es2015

/*
    1
*/
interface i1 {
    name: string;
}

class C1 implements i1 {
    public name:string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class C2 implements i1 {
  //~^ ERROR: Class 'C2' incorrectly implements interface 'i1'.
    private name:string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class C3 {
    private name:any;
}

class C4 extends C1 implements i1{ }
class C5 extends C2 implements i1{ }
//~^ ERROR: Class 'C5' incorrectly implements interface 'i1'.
class C6 extends C3 implements i1{ }
//~^ ERROR: Class 'C6' incorrectly implements interface 'i1'.

/*
    2
*/

interface i2 {
    name: string;
    age: number;
}

class C7 {
    public name:string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class C8 extends C7 implements i2{
    public age:number;
    //~^ ERROR: Property 'age' has no initializer and is not definitely assigned in the constructor.
}