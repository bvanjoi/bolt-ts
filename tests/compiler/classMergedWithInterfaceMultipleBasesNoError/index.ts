// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMergedWithInterfaceMultipleBasesNoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Bar { }
interface Baz { }
interface Q { }
interface Foo extends Bar, Baz { }
class Foo { }

export default class extends Foo {
    readonly observer = this.handleIntersection;
    //~^ ERROR: Property 'handleIntersection' is used before its initialization.
    readonly handleIntersection = () => { }
}
