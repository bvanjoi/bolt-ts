// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classExtendsMultipleBaseClasses.ts`, Apache-2.0 License

class A { }
class B { }
class C extends A, B { }
//~^ ERROR: Syntax Error: Unexpected token ','