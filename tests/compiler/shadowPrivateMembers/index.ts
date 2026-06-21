// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/shadowPrivateMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class base { private n() {} }
class derived extends base { private n() {} }
//~^ ERROR: Class 'derived' incorrectly extends base class 'base'.
