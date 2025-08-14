// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/extendPrivateConstructorClass.ts`, Apache-2.0 License

declare namespace abc {
    class XYZ {
        private constructor();
    }
}

class C extends abc.XYZ { //~ ERROR: Cannot extend a class 'XYZ'. Class constructor is marked as private.
}

class ABC {
  private constructor() {
    class C3 extends ABC {}
  }
}

class C2 extends ABC {  //~ ERROR: Cannot extend a class 'ABC'. Class constructor is marked as private.
}