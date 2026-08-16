// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asiAbstract.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

abstract
//~^ ERROR: Cannot find name 'abstract'
class NonAbstractClass {
  abstract s();
  //~^ ERROR: Abstract modifier can only appear within an abstract class.
}

class C2 {
    abstract
    nonAbstractFunction() {
    }
}

class C3 {
    abstract
}
