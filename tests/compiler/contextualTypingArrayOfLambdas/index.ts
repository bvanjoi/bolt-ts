// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypingArrayOfLambdas.ts`, Apache-2.0 License

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
