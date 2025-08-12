// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/asiAbstract.ts`, Apache-2.0 License

abstract
//~^ ERROR: Cannot find name 'abstract'
class NonAbstractClass {
  abstract s();
  //~^ ERROR: Abstract methods can only appear within an abstract class.
}

class C2 {
    abstract
    nonAbstractFunction() {
    }
}

class C3 {
    abstract
}
