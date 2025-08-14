// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/numberToString.ts`, Apache-2.0 License

function f1(n:number):string {
  return n; // error return type mismatch
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

function f2(s:string):void {
}

f1(3);
f2(3); // error no coercion to string
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
f2(3+""); // ok + operator promotes
