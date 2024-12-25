// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/extendNonClassSymbol1.ts`, Apache-2.0 License

class A { foo() { } }
var x = A;
class C extends x { } // error, could not find symbol xs