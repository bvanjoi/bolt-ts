// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ClassTest5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C1T5 {
    foo: (i: number, s: string) => number = 
	   	(i) => {
	        return i;
	    }
}
namespace C2T5 {}

class  bigClass {
     public break = 1;
}
