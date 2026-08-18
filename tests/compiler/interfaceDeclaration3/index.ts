// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceDeclaration3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

interface I1 { item:number; }

namespace M1 {
    interface I1 { item:string; }  
    interface I2 { item:number; }   
    class C1 implements I1 {
        public item:number;
        //~^ ERROR: Property 'item' in type 'C1<C1>' is not assignable to the same property in base type 'I1'.
        //~| ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
    class C2 implements I1 {
        public item:string;
        //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
    class C3 implements I2 {
        public item:number;
        //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
    
    class C4 implements M2.I1 { 
        public item:string;
        //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }

    class C5 implements M2.M3.I1 {
        public item:string;
        //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
}

export namespace M2 {
    export interface I1 { item:string; }
    export interface I2 { item:string; }
    export namespace M3 {
        export interface I1 { item:string; }
    }
    class C1 implements I1 {
        public item:number;    
        //~^ ERROR: Property 'item' in type 'C1<C1>' is not assignable to the same property in base type 'M2.I1'.
        //~| ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
    class C2 implements I1 {
        public item:string;    
        //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
    class C3 implements I2 {
        public item:string;    
        //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
    }
}

class C1 implements I1 {
    public item:number;
    //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
}

class C2 implements M2.I1 { 
    public item:string;
    //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
}

class C3 implements M2.M3.I1 {
    public item:string;
    //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
}

interface I2 extends I1 { item:string; }
//~^ ERROR: Interface 'I2' incorrectly extends interface 'I1'.