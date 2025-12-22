// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classPropertyErrorOnNameOnly.ts`, Apache-2.0 License

//@compiler-options: strict

type Values = 1 | 2 | 3 | 4 | 5 | 6

type FuncType = (arg: Values) => string

// turn on strictNullChecks
class Example {
  insideClass: FuncType = function(val) { // error span goes from here
//~^ ERROR: Type '(val: any) => undefined | "1" | "2" | "3" | "4" | "5"' is not assignable to type '(arg: 1 | 2 | 3 | 4 | 5 | 6) => string'.
    switch (val) {
      case 1:
        return "1";
      case 2:
        return "2";
      case 3:
        return "3"
      case 4:
        return "4"
      case 5:
        return "5"
      // forgot case 6
    }
  } // all the way to here
}

const outsideClass: FuncType = function(val) { // compare to errors only on this line in this case 
//~^ ERROR: Type '(val: 1 | 2 | 3 | 4 | 5 | 6) => undefined | "1" | "2" | "3" | "4" | "5"' is not assignable to type '(arg: 1 | 2 | 3 | 4 | 5 | 6) => string'.
    switch (val) {
      case 1:
        return "1";
      case 2:
        return "2";
      case 3:
        return "3"
      case 4:
        return "4"
      case 5:
        return "5"
      // forgot case 6
    }
}