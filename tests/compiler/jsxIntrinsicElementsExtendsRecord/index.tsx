// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsxIntrinsicElementsExtendsRecord.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: jsx=preserve

declare namespace JSX {
  interface IntrinsicElements extends Record<string, any> {}
}

<a />;
