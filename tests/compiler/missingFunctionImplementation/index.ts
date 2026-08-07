// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingFunctionImplementation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false


export class C1 {
  m(): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}

// merged with a namespace
export class C2 {
  m(): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
export namespace C2 { }


// merged with a namespace, multiple overloads
class C3 {
  m(a, b);
  m(a);
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
namespace C3 { }

// static methods, multiple overloads
class C4 {
  static m(a): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}

// static methods, multiple overloads
class C5 {
  static m(a): void;
  static m(): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}

// merged with namespace, static methods
class C6 {
  static m(): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
namespace C6 {
}

// merged with namespace, static methods, multiple overloads
class C7 {
  static m(a): void;
  static m(): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
namespace C7 {
}

// merged with namespace, static methods, duplicate declarations
class C8 {
  static m(a): void;
  static m(a, b): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
namespace C8 {
  export function m(a?, b?): void { }
  //~^ ERROR: Duplicate identifier 'm'.
}

// merged with namespace, static methods, duplicate declarations
class C9 {
  static m(a): void { }
}
namespace C9 {
  export function m(a): void;
  //~^ ERROR: Duplicate identifier 'm'.
  //~| ERROR: Function implementation is missing or not immediately following the declaration.
  //~| ERROR: Function implementation is missing or not immediately following the declaration.
}

// merged namespaces
namespace N10 {
  export function m(a): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
namespace N10 {
  export function m(a): void { }
}

// merged namespaces, duplicate defintions
namespace N12 {
  export function m(a): void;
  //~^ ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  export function m(): void;
  //~^ ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  export function m(a?): void { }
  //~^ ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
}
namespace N12 {
  export function m(a): void { }
  //~^ ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
  //~| ERROR: Duplicate function implementation.
}
