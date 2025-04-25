// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/taggedTemplatesInDifferentScopes.ts`, Apache-2.0 License

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
