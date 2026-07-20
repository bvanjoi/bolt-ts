// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arityErrorRelatedSpanBindingPattern.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(a, b, {c}): void {}

function bar(a, b, [c]): void {}

foo("", 0);
//~^ ERROR: Expected 3 arguments, but got 2.
bar("", 0);
//~^ ERROR: Expected 3 arguments, but got 2.
