// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveInheritance.ts`, Apache-2.0 License

interface I5 extends I5 { // error
//~^ ERROR: Type 'I5' recursively references itself as a base type.
  foo():void;
} 

interface i8 extends i9 { } // error
//~^ ERROR: Type 'i8' recursively references itself as a base type.
interface i9 extends i8 { } // error
//~^ ERROR: Type 'i9' recursively references itself as a base type.
