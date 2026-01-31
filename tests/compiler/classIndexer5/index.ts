// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classImplementsPrimitive.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: useDefineForClassFields=false

class Foo {
    [key: string]: number;

    #a: boolean;
    #b = false;
}
