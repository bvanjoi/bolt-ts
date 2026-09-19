// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classFieldSuperAccessibleJs2.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: checkJs
//@compiler-options: target=esnext
//@compiler-options: noEmit

class C {
  constructor() {
    this.foo = () => {
      console.log("called arrow");
    };
  }
  foo() {
    console.log("called method");
  }
}

class D extends C {
  foo() {
    console.log("SUPER:");
    super.foo();
    console.log("THIS:");
    this.foo();
  }
}

const obj = new D();
obj.foo();
D.prototype.foo.call(obj);
