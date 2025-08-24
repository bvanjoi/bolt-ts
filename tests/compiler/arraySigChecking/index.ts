// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/arraySigChecking.ts`, Apache-2.0 License

declare module M {
  interface iBar { t: any; }
  interface iFoo extends iBar {
      s: any;
  }

  class cFoo {
      t: any;
  }

  var foo: { [index: any]; }; // expect an error here
  //~^ ERROR: An index signature must have a type annotation.
  //~| ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
}

interface myInt {
  voidFn(): void;
}
var myVar: myInt;
var strArray: string[] = [myVar.voidFn()];
//~^ ERROR: Type 'void' is not assignable to type 'string'.

var myArray: number[][][];
myArray = [[1, 2]];
//~^ ERROR: Type 'number' is not assignable to type 'number[]'.
//~| ERROR: Type 'number' is not assignable to type 'number[]'.

function isEmpty(l: { length: number }) {
  return l.length === 0;
}

isEmpty([]);
isEmpty(new Array(3));
isEmpty(new Array<string>(3));
isEmpty(['a']);
