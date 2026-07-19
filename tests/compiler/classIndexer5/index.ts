// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classIndexer5.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: useDefineForClassFields=false

class Foo {
    [key: string]: number;

    #a: boolean;
    //~^ ERROR: Property '##a' has no initializer and is not definitely assigned in the constructor.
    #b = false;
}
