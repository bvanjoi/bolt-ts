// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericSpecializations3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IFoo<T> {
    foo(x: T): T;
}

declare var iFoo: IFoo<number>;
iFoo.foo(1);

class IntFooBad implements IFoo<number> { // error
    foo(x: string): string { return null; }
    //~^ ERROR: Property 'foo' in type 'IntFooBad<IntFooBad>' is not assignable to the same property in base type 'IFoo<number, IntFooBad>'.
    //~| ERROR: Type 'null' is not assignable to type 'string'.
}

declare var intFooBad: IntFooBad;

class IntFoo implements IFoo<number> {
    foo(x: number): number { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'number'.
}

declare var intFoo: IntFoo;

class StringFoo2 implements IFoo<string> {
    foo(x: string): string { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'string'.
}

declare var stringFoo2: StringFoo2;
stringFoo2.foo("hm");


intFoo = stringFoo2; // error
//~^ ERROR: Type 'StringFoo2' is not assignable to type 'IntFoo'.
stringFoo2 = intFoo; // error
//~^ ERROR: Type 'IntFoo' is not assignable to type 'StringFoo2'.

class StringFoo3 implements IFoo<string> { // error
    foo<T>(x: T): T { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'T'.
}
var stringFoo3: StringFoo3;
