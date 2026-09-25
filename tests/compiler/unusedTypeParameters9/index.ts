// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameters9.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

// clas + interface
class C1<T> { }
interface C1<T> { a: T; }

// interface + class
class C2<T> { a: T; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
interface C2<T> { }

// interfaces
interface C3<T> { a(c: (p: T) => void): void; }
interface C3<T> { b: string; }
interface C3<T> { c: number; }
interface C3<T> { d: boolean;  }
interface C3<T> { e: any; }