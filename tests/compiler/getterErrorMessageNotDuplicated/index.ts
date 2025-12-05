// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/getterErrorMessageNotDuplicated.ts`, Apache-2.0 License

interface Thing {
    get style(): Foo;
    set style(cssText: string | Bar);
}

interface Foo {
    hello: string;
    world: number;
}

interface Bar extends Foo {
    extra: any;
}