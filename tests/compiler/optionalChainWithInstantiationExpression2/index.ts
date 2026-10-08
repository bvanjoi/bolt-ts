// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalChainWithInstantiationExpression2.ts`, Apache-2.0 License

//@[target=es2019]  compiler-options: target=es2019
//@[target=es2020]  compiler-options: target=es2020
//@run-fail

declare interface A {
    c: number;
    <T>(): T;
}

type b = 'b type';

declare const a: A | undefined;

a?.<b>();

a<b>?.();