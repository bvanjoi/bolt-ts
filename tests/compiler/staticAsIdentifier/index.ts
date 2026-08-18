// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticAsIdentifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A {
    static
    b: number = 2;
}
A.b;


class C1 {
    static static
    [x: string]: string;
}

class C2 {
    static static
    m() {}
}

class C3 {
    static static p: string;
    //~^ ERROR: Unexpected keyword or identifier.
}

class C4 {
    static static foo() {}
    //~^ ERROR: Unexpected keyword or identifier.
}

class C5 {
    static static
}

class C6 {
    static 
    static
}

class C7 extends C6 {
    static override static
}
