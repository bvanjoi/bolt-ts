// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambientModuleWithTemplateLiterals.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare namespace Foo {
    enum Bar {
        a = `1`,
        b = '2',
        c = '3'
    }

    export const a = 'string';
    export const b = `template`;

    export const c = Bar.a;
    export const d = Bar['b'];
    export const e = Bar[`c`];
}

Foo.a;
Foo.b;
Foo.c;
Foo.d;
Foo.e;