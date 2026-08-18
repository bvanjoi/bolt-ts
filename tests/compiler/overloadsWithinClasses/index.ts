// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadsWithinClasses.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

class foo {
 
    static fnOverload( ) {}
    //~^ ERROR: Duplicate function implementation.
 
    static fnOverload(foo: string){ } // error
    //~^ ERROR: Duplicate function implementation.
 
}

class bar {
 
    static fnOverload( );
 
    static fnOverload(foo?: string){ } // no error
 
}

class X {
   public attr(name:string):string;
   public attr(name:string, value:string):X;
   public attr(first:any, second?:any):any {
   }
}
