// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arityErrorRelatedSpanBindingPattern.ts`, Apache-2.0 License

function foo(a, b, {c}): void {}

function bar(a, b, [c]): void {}

foo("", 0);
//~^ ERROR: Expected 3 arguments, but got 2.
bar("", 0);
//~^ ERROR: Expected 3 arguments, but got 2.
