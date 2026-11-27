// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplatesInDifferentScopes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export function tag(parts: TemplateStringsArray, ...values: any[]) {
  return parts[0];
}
function foo() {
  tag `foo`;
  tag `foo2`;
}

function bar() {
  tag `bar`;
  tag `bar2`;
}

foo();
bar();


{
  function f<T>(a: any) {}
  f<number>``;
}
