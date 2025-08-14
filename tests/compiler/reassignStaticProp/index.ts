// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/reassignStaticProp.ts`, Apache-2.0 License

class foo {
 
    static bar = 1;
 
    static bar:string; // errror - duplicate id
    //~^ ERROR: Duplicate identifier 'bar'.
 
}
 
 

