// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forwardRefInTypeDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@[strict=true]  compiler-options: strict=true
//@[strict=false] compiler-options: strict=false

// forward ref ignored in a typeof
declare let s: typeof s1;
const s1 = "x";

// ignored anywhere in an interface (#35947)
interface Foo2 { [s2]: number; }
const s2 = "x";

// or in a type definition
type Foo3 = { [s3]: number; }
const s3 = "x";

// or in a type literal
declare const foo4: { [s4]: number; }
const s4 = "x";

// or in a declared class
declare class Foo5 { [s5]: number; }
const s5 = "x";

// or with qualified names
interface Foo6 { [Cls1.a]: number; [Cls2.b]: number; [obj1.c]: number; [obj2.d]: number }
declare class Cls1 { static a: "a"; }
class Cls2 { static b = "b" as const; }
declare const obj1: { c: 'c' }
const obj2 = { d: 'd' } as const
