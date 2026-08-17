// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidOptionalChainFromNewExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    b() {}
}

new A?.b()   // error
//~^ ERROR: Invalid optional chain from new expression.
new A()?.b() // ok
