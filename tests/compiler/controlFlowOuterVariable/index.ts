// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowOuterVariable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

const CONFIG = {
    foo: '',
    setFoo: function(foo: string) {
        CONFIG.foo = foo;
    }
};

const helper = function<T>(t: T[]) {
    helper(t.slice(1));
}