// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitPropertyNumericStringKey.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

// https://github.com/microsoft/TypeScript/issues/55292

const STATUS = {
    ["404"]: "not found",
} as const;

const hundredStr = "100";
const obj = { [hundredStr]: "foo" };

const hundredNum = 100;
const obj2 = { [hundredNum]: "bar" };
