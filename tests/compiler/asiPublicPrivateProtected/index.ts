// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/asiInES6Classes.ts`, Apache-2.0 License

public
//~^ ERROR: Cannot find name 'public'
class NonPublicClass {
    public s() {
    }
}

class NonPublicClass2 {
    public
    private nonPublicFunction() {
    }
}
private
//~^ ERROR: Cannot find name 'private'
class NonPrivateClass {
    private s() {
    }
}

class NonPrivateClass2 {
    private
    public nonPrivateFunction() {
    }
}
protected
//~^ ERROR: Cannot find name 'protected'
class NonProtectedClass {
  protected s() {
  }
}

class NonProtectedClass2 {
    protected
    public nonProtectedFunction() {
    }
}

class ClassWithThreeMembers {
    public
    private
    protected
}
