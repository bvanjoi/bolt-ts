// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classExtendsInterfaceInExpression.ts`, Apache-2.0 License

interface A {}

function factory(a: any): {new(): Object} {
  return null;
}

class C extends factory(A) {}
//~^ ERROR: Cannot find name 'A'.
