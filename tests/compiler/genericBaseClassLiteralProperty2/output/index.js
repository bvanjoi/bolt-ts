// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericBaseClassLiteralProperty2.ts`, Apache-2.0 License
class CollectionItem2 {}
class BaseCollection2 {
  _itemsByKey;
  constructor() {this._itemsByKey = {};}
}
class DataView2 extends BaseCollection2 {
  fillItems(item) {
    this._itemsByKey['dummy'] = item;
  }
}