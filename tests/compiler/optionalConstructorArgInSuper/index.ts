// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalConstructorArgInSuper.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

class Base {
    constructor(opt?) { }
    foo(other?) { }
}
class Derived extends Base {
}
var d = new Derived(); // bug caused an error here, couldn't select overload
var d2: Derived;
d2.foo(); 
