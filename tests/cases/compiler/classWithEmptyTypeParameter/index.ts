// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/classWithEmptyTypeParameter.ts`, Apache-2.0 License

class C<> { //~ERROR: Type parameter list cannot be empty.
}