// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorWithTruncatedType.ts`, Apache-2.0 License

//@compiler-options: target=es2020
//@compiler-options: noErrorTruncation

declare var x: {
    propertyWithAnExceedinglyLongName1: string;
    propertyWithAnExceedinglyLongName2: string;
    propertyWithAnExceedinglyLongName3: string;
    propertyWithAnExceedinglyLongName4: string;
    propertyWithAnExceedinglyLongName5: string;
};

// String representation of type of 'x' should be truncated in error message
var s: string = x;
//~^ ERROR: Type '{ propertyWithAnExceedinglyLongName1: string; propertyWithAnExceedinglyLongName2: string; propertyWithAnExceedinglyLongName3: string; propertyWithAnExceedinglyLongName4: string; propertyWithAnExceedinglyLongName5: string; }' is not assignable to type 'string'.