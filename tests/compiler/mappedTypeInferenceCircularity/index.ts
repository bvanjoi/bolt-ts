// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeInferenceCircularity.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

type HTML = { [K in 'div']: Block<HTML> };
type Block<P> = <T>(func: HTML) => {};

declare var h: HTML;
h.div(h);