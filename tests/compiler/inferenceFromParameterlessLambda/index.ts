// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceFromParameterlessLambda.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T>(o: Take<T>, i: Make<T>) { }
interface Make<T> {
    (): T;
}
interface Take<T> {
    (n: T): void;
}
// Infer string from second argument because it isn't context sensitive
foo(n => n.length, () => 'hi');
