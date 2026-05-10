// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedTypeVariableInfersLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitThis

type Foo = any;

const foo: Foo = {};

foo.bar = function () {
    const self: Foo = this;
};

foo.zab = (function () {
    const self: Foo = this;
});
