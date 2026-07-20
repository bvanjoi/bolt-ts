// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericConstraintDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class List<T extends {}>{
    static empty<T extends {}>(): List<T>{return null;}
    //~^ ERROR: Type 'null' is not assignable to type 'List<T>'.
}