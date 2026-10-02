// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericWithIndexerOfTypeParameterType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

export class Collection<TItem extends CollectionItem> {
    _itemsByKey: { [key: string]: TItem; };
    //~^ ERROR: Property '_itemsByKey' has no initializer and is not definitely assigned in the constructor.
}

export class List extends Collection<ListItem>{
    Bar() {}
}

export class CollectionItem {}

export class ListItem extends CollectionItem {
    __isNew: boolean;
    //~^ ERROR: Property '__isNew' has no initializer and is not definitely assigned in the constructor.
}
