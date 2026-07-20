// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumIndexer.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

enum MyEnumType {
    foo, bar
}
var _arr = [{ key: 'foo' }, { key: 'bar' }]
var enumValue = MyEnumType.foo;
var x = _arr.map(o => MyEnumType[o.key] === enumValue); // these are not same type
