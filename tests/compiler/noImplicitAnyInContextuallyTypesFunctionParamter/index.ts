// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyInContextuallyTypesFunctionParamter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

var regexMatchList = ['', ''];
regexMatchList.forEach(match => ''.replace(match, ''));
