// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritanceGrandParentPublicMemberCollisionWithPrivateMember.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A {
    public myMethod() { }
}

class B extends A { }

class C extends B {
  //~^ ERROR: Class 'C' incorrectly extends base class 'B'.
    private myMethod() { }
}
