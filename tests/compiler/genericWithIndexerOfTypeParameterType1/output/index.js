class LazyArray {
  objects = ({});
  array() {
    return this.objects;
  }
}
var lazyArray = new LazyArray();
var value = lazyArray.array()['test'];