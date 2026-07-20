// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingOfConditionalExpression2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A {
    foo: number;
}
class B extends A {
    bar: number;
}
class C extends A {
    baz: number;
}

var x2: (a: A) => void = true ? (a: C) => a.foo : (b: number) => { };
//~^ ERROR: Type '((a: C) => number) | ((b: number) => void)' is not assignable to type '(a: A) => void'.
