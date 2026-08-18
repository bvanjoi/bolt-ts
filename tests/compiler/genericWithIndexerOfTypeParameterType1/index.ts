// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericWithIndexerOfTypeParameterType1.ts`, Apache-2.0 License

class LazyArray<T> {
    private objects = <{ [objectId: string]: T; }>{};
    array() {
        return this.objects;
    }
}
var lazyArray = new LazyArray<string>();
var value: string = lazyArray.array()["test"]; // used to be an error
