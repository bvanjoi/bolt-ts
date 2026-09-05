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