// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/assignmentCompatOnNew.ts`, Apache-2.0 License

class Foo{};

function bar(x: {new(): Foo;}){}

bar(Foo); // Error, but should be allowed
