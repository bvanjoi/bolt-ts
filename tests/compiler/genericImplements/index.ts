// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericImplements.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false


class A { a; };
class B { b; };
interface I {
    f<T extends A>(): T;
} // { f: () => { a; } }

// OK
class X implements I {  
    f<T extends B>(): T { return undefined; }
    //~^ ERROR: Property 'f' in type 'X<X>' is not assignable to the same property in base type 'I'.
} // { f: () => { b; } }

// OK
class Y implements I {
    f<T extends A>(): T { return undefined; }
} // { f: () => { a; } }

// OK
class Z implements I {
    f<T>(): T { return undefined; }
} // { f: <T>() => T } 