// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/anonterface.ts`, Apache-2.0 License

module M {
  export class C {
      m(fn:{ (n:number):string; },n2:number):string {
          return fn(n2);
      }
  }
}

var c=new M.C();
c.m(function(n) { return "hello: "+n; },18);
c.m(function(n) { return 42; },18);
//~^ ERROR: Argument of type '(n: number) => void' is not assignable to parameter of type '(n: number) => string'.
c.m(function(n) { return "hello: "+n; }, '42');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
