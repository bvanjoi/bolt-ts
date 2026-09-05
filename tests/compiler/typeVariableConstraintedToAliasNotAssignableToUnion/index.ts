// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeVariableConstraintedToAliasNotAssignableToUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare class TableClass<S = any> {
    _field: S;
}

export type Table = TableClass;

interface Something {
    prop: number;
}

interface SomethingElse {
    prop2: string;
}

declare let aBoolean: boolean;
declare let aStringOrNumber: string | number;
declare let aStringOrSomething: string | Something;
declare let someUnion: Something | SomethingElse;

function fn<T extends Table>(o: T) {
    aBoolean = o;
    //~^ ERROR: Type 'T' is not assignable to type 'boolean'.
    aStringOrNumber = o;
    //~^ ERROR: Type 'T' is not assignable to type 'number | string'.
    aStringOrSomething = o;
    //~^ ERROR: Type 'T' is not assignable to type 'string | Something'.
    someUnion = o;
    //~^ ERROR: Type 'T' is not assignable to type 'Something | SomethingElse'.
}

function fn2<T extends TableClass>(o: T) {
    aBoolean = o;
    //~^ ERROR: Type 'T' is not assignable to type 'boolean'.
    aStringOrNumber = o;
    //~^ ERROR: Type 'T' is not assignable to type 'number | string'.
    aStringOrSomething = o;
    //~^ ERROR: Type 'T' is not assignable to type 'string | Something'.
    someUnion = o;
    //~^ ERROR: Type 'T' is not assignable to type 'Something | SomethingElse'.
}

declare const o: Table;
aBoolean = o;
//~^ ERROR: Type 'Table' is not assignable to type 'boolean'.
aStringOrNumber = o;
//~^ ERROR: Type 'Table' is not assignable to type 'number | string'.
aStringOrSomething = o;
//~^ ERROR: Type 'Table' is not assignable to type 'string | Something'.
someUnion = o;
//~^ ERROR: Type 'Table' is not assignable to type 'Something | SomethingElse'.
