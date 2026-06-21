// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ParameterList8.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare class C2 {
 constructor(public p1:string); // ERROR
 //~^ ERROR: A parameter property is only allowed in a constructor implementation.
 constructor(private p2:number); // ERROR
 //~^ ERROR: A parameter property is only allowed in a constructor implementation.
 constructor(public p3:any); // ERROR
 //~^ ERROR: A parameter property is only allowed in a constructor implementation.
}