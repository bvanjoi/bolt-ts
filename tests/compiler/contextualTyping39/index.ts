// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping39.ts`, Apache-2.0 License

var foo = <{ (): number; }> function() { return "err"; };
//~^ ERROR: Conversion of type '() => string' to type '() => number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
