// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminantUsingEvaluatableTemplateExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type S = { d: "s"; cb: (x: string) => void };
type N = { d: "n"; cb: (x: number) => void };

declare function foo(foo: S | N): void;

foo({
  d: `${"s"}`,
  cb: (x) => {
    x; // string
  },
});