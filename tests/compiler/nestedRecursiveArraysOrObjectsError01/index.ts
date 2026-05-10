// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedRecursiveArraysOrObjectsError01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Style = StyleBase | StyleArray;
interface StyleArray extends Array<Style> {}
interface StyleBase {
    foo: string;
}

const blah: Style = [
    [[{
        foo: 'asdf',
        jj: 1 // intentional error
        //~^ ERROR: Object literal may only specify known properties, and 'jj' does not exist in type 'Style'.
    }]]
];

