// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericBaseClassLiteralProperty2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class CollectionItem2 { }

class BaseCollection2<TItem extends CollectionItem2> {
    _itemsByKey: { [key: string]: TItem; };
    constructor() {
        this._itemsByKey = {};
    }
}

class DataView2 extends BaseCollection2<CollectionItem2> {
    fillItems(item: CollectionItem2) {
        this._itemsByKey['dummy'] = item;
    }
}
