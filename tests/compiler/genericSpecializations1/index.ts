// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericSpecializations1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IFoo<T> {
    foo<T>(x: T): T; // no error on implementors because IFoo's T is different from foo's T
}

class IntFooBad implements IFoo<number> {
    foo(x: string): string { return null; }
    //~^ ERROR: Property 'foo' in type 'IntFooBad<IntFooBad>' is not assignable to the same property in base type 'IFoo<number, IntFooBad>'.
    //~| Type 'null' is not assignable to type 'string'.
}

class StringFoo2 implements IFoo<string> {
    foo(x: string): string { return null; }
    //~^ ERROR: Property 'foo' in type 'StringFoo2<StringFoo2>' is not assignable to the same property in base type 'IFoo<string, StringFoo2>'.
    //~| Type 'null' is not assignable to type 'string'.
}

class StringFoo3 implements IFoo<string> {
    foo<T>(x: T): T { return null; }
    //~^ Type 'null' is not assignable to type 'T'.
}
