// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByParenthesizedSwitchExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

interface Base {
  type: "foo" | "bar";
}

interface Foo extends Base {
  type: "foo";
  foo: string;
}

interface Bar extends Base {
  type: "bar";
  bar: number;
}

function getV(): Foo | Bar {
  return null!;
}

const v = getV();

switch ((v.type)) {
  case "bar":
    v.bar;
    break;

  case "foo":
    v.foo;
    break;
}
