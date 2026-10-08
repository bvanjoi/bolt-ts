// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericWithIndexerOfTypeParameterType1.ts`, Apache-2.0 License
class LazyArray {
  objects = ({});
  array() {
    return this.objects;
  }
}
var lazyArray = new LazyArray();
var value = lazyArray.array()['test'];