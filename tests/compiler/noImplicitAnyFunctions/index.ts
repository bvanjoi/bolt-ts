// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyFunctions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare function f1();
//~^ ERROR: 'f1', which lacks return-type annotation, implicitly has an 'any' return type.

declare function f2(): any;

function f3(x) {
  //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
}

function f4(x: any) {
    return x;
}

function f5(x: any): any {
    return x;
}

function f6(x: string, y: number);
//~^ ERROR: 'f6', which lacks return-type annotation, implicitly has an 'any' return type.
function f6(x: string, y: string): any;
function f6(x: string, y) {
  //~^ ERROR: Parameter 'y' implicitly has an 'any' type.
    return null;
}