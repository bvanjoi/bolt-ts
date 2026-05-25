class Test {
  constructor() {}
  set value(value) {}
  get value() {
    return null
  }
}
var a = new Test();
a.value = (item) => (item.property);
a['value'] = (item) => (item.property);