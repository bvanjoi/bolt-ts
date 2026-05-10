// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionOfMixinConstructorTypeAndNonConstructorType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare let x: {foo: undefined} & {new(...args: any[]): any};
new x();
