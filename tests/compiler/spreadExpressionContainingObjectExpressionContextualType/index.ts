// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadExpressionContainingObjectExpressionContextualType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noEmit

// repro #49585

const { value } = (() => ({
  value: "",
  ...(true ? {} : {}),
}))();

// repro 49684#discussion_r920545763

const { value2 } = {
  value2: "",
  ...(() => true ? {} : {})(),
};
