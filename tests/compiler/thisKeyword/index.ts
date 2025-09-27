// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/thisKeyword.ts`, Apache-2.0 License

namespace foo {
    this.bar = 4;
    //~^ ERROR: 'this' cannot be referenced in a module or namespace body.
}