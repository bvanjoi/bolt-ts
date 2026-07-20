// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/knockout.ts`, Apache-2.0 License

//@compiler-options: target=es2015

  declare namespace ko {
   export interface Observable<T> {
     (): T;
     (value: T): any;
     N: number;
     g: boolean;
     r: T;
   }
   export function observable<T>(value: T): Observable<T>;
 }
 var o = {
     name: ko.observable("Bob"),
     age: ko.observable(37)
 }
 var x_v = o.name().length
 var age_v = o.age();
 var name_v = o.name("Robert");
 var zz_v = o.name.N;
 var yy_v = o.name.g;
 var rr_v = o.name.r;
 var dd_v = o.name.d;
 //~^ ERROR: Property 'd' does not exist on type 'ko.Observable<string>'.