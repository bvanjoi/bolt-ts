// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/autonumberingInEnums.ts`, Apache-2.0 License

enum Foo {
    a = 1
}

enum Foo {
    b // should work fine
}