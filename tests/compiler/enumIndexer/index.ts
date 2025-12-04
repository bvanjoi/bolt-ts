// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/enumIndexer.ts`, Apache-2.0 License

enum MyEnumType {
    foo, bar
}
var _arr = [{ key: 'foo' }, { key: 'bar' }]
var enumValue = MyEnumType.foo;
var x = _arr.map(o => MyEnumType[o.key] === enumValue); // these are not same type
