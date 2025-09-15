// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/genericCallWithFixedArguments.ts`, Apache-2.0 License

class A { foo() { } }
class B { bar() { }} 

function g<T, U>(x) { }
g<A, B>(7) // the parameter list is fixed, so this should not error
