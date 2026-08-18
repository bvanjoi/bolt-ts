// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalTypeBasedContextualTypeReturnTypeWidening.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var func1 = useState1(() => (() => (0)));
var func2 = useState2(() => (() => (0)));
var func3 = useState1(() => (() => (0)));
var func4 = useState2(() => (() => (0)));