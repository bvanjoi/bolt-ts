// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mutuallyRecursiveGenericBaseTypes2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class foo<T>
{
    bar(): foo2<T[]> { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'foo2<T[]>'.
}
 
class foo2<T> extends foo<T> {
}
 
var test = new foo<string>();