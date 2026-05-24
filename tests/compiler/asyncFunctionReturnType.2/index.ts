// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncFunctionReturnType.2.ts`, Apache-2.0 License

//@compiler-options: target=esnext

class X {
    f = async (): Promise<this> => this;
}