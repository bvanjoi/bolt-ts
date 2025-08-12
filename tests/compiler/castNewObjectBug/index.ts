// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/castNewObjectBug.ts`, Apache-2.0 License

interface Foo { }
var xx = <Foo> new Object();