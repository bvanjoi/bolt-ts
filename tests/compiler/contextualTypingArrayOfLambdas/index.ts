// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingArrayOfLambdas.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A {
    foo: string;
}

class B extends A {
    bar: string;
}

class C extends A {
    baz: string;
}

var xs = [(x: A) => { }, (x: B) => { }, (x: C) => { }];
