// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forOfTransformsExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// https://github.com/Microsoft/TypeScript/issues/11024
let items = [{ name: "A" }, { name: "C" }, { name: "B" }];
for (var item of items.sort((a, b) => a.name.localeCompare(b.name))) {

}
