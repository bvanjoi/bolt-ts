// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceofTypeAliasToGenericClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare class TableClass<S = any> {
    _field: S;
}

export type Table = TableClass;

function fn<T extends Table>(o: T) {
    return o instanceof TableClass;
}

function fn2<T extends TableClass>(o: T) {
    return o instanceof TableClass;
}

declare const o: Table;
o instanceof TableClass;
