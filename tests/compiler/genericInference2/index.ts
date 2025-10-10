// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericInference2.ts`, Apache-2.0 License

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
    }; 
    var x_v = o.name().length;  // should be 'number'
    
    var age_v = o.age();  // should be 'number'
    
    var name_v = o.name("Robert");  // should be 'any'
    var zz_v = o.name.N;  // should be 'number'
    var yy_v = o.name.g;  // should be 'boolean'
    var rr_v = o.name.r;  // should be 'string'

var x_v1: string = o.name().length;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var age_v1: string = o.age();
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var name_v1: string = o.name("Robert");
var name_v2: number = o.name("Robert");
var zz_v1: string = o.name.N;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var yy_v1: string = o.name.g;
//~^ ERROR: Type 'boolean' is not assignable to type 'string'.
var rr_v1: number = o.name.r;
//~^ ERROR: Type 'string' is not assignable to type 'number'.