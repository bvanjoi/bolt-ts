// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericMappedTypeAsClause.ts`, Apache-2.0 License

//@compiler-options: target=es2020
//@compiler-options: strict

type Model = {
    a: string;
    b: number;
};

type MappedModel<Suffix extends string> = {
    [K in keyof Model as `${K}${Suffix}`]: Model[K];
};

const foo1: MappedModel<'Foo'> = { aFoo: 'test', bFoo: 42 };
const foo2: MappedModel<'Foo'> = { bFoo: 'bar' };  // Error
//~^ ERROR: Type 'string' is not assignable to type 'number'.

function f1<T extends string>() {
    const x1: MappedModel<T> = 42;  // Error
    //~^ ERROR: Type 'number' is not assignable to type 'MappedModel<T>'.
    const x2: MappedModel<T> = 'test';  // Error
    //~^ ERROR: Type 'string' is not assignable to type 'MappedModel<T>'.
    const x3: MappedModel<T> = [1, 2, 3];  // Error
    //~^ ERROR: Type 'number[]' is not assignable to type 'MappedModel<T>'.
    const x4: MappedModel<T> = false;  // Error
    //~^ ERROR: Type 'boolean' is not assignable to type 'MappedModel<T>'.
    const x5: MappedModel<T> = { a: 'bar', b: 42 };  // Error
    //~^ ERROR: Object literal may only specify known properties, and 'a' does not exist in type 'MappedModel<T>'.
    const x6: MappedModel<T> = undefined;  // Error
    //~^ ERROR: Type 'undefined' is not assignable to type 'MappedModel<T>'.
}

// repro from #43189

type RemapRecord<K extends keyof any, V> = { [_ in never as K]: V }
interface RecordInterface2<K extends keyof any, V> extends RemapRecord<K, V> {} // should error
//~^ ERROR: An interface can only extend an object type or intersection of object types with statically known members.
