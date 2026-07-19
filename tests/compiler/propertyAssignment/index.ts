// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare var foo1: { new ():any; }   
declare var bar1: { x : number; }

declare var foo2: { [index]; } // should be an error, used to be indexer, now it is a computed property
//~^ ERROR: Cannot find name 'index'.
declare var bar2: { x : number; }

declare var foo3: { ():void; }
declare var bar3: { x : number; }



foo1 = bar1; // should be an error
//~^ ERROR: Type '{ x: number; }' is not assignable to type 'new () => any'.
foo2 = bar2; 
foo3 = bar3; // should be an error
//~^ ERROR: Type '{ x: number; }' is not assignable to type '() => void'.
