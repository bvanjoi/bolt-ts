// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/vararg.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
    export class C {
        public f(x:string,...rest:number[]) {
            var sum=0;
            for (var i=0;i<rest.length;i++) {
                sum+=rest[i];
            }
            result+=(x+": "+sum);
            return result;
        }

        public fnope(x:string,...rest:number) {
          //~^ ERROR: A rest parameter must be of an array type.
    
        }

        public fonly(...rest:string[]) {
            builder="";
            //~^ ERROR: Cannot find name 'builder'.
            for (var i=0;i<rest.length;i++) {
                builder+=rest[i];
            //~^ ERROR: Cannot find name 'builder'.
            }
            return builder;
            //~^ ERROR: Cannot find name 'builder'.
        }
    }
}

var x=new M.C();
var result="";
result+=x.f(x,3,3); // bad first param
//~^ ERROR: Argument of type 'M.C' is not assignable to parameter of type 'string'.
result+=x.f(3,"hello",3); // bad second param
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
result+=x.f("hello",3,3,3,3,3); // ok
result+=x.f("hello"); // ok varargs length 0
result+=x.fonly(3); // ok conversion
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
result+=x.fonly(x); // bad param
//~^ ERROR: Argument of type 'M.C' is not assignable to parameter of type 'string'.
result+=x.fonly("a"); // ok 
result+=x.fonly("a","b","c","d"); //ok 


