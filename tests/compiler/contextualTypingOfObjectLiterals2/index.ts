// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/contextualTypingOfObjectLiterals2.ts`, Apache-2.0 License

interface Foo {
  foo: (t: string) => string;
}
function f2(args: Foo) { }
f2({ foo: s => s.hmm }) // 's' should be 'string', so this should be an error
//~^ ERROR: Property 'hmm' does not exist on type 'string'.