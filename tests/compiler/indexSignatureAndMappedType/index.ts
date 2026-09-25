// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexSignatureAndMappedType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

// A mapped type { [P in K]: X }, where K is a generic type, is related to
// { [key: string]: Y } if X is related to Y.

function f1<T, K extends string>(x: { [key: string]: T }, y: Record<K, T>) {
    x = y;
    y = x;  // Error
    //~^ ERROR: Type '{ [key: string]: T }' is not assignable to type 'Record<K, T>'.
}

function f2<T>(x: { [key: string]: T }, y: Record<string, T>) {
    x = y;
    y = x;
}

function f3<T, U, K extends string>(x: { [key: string]: T }, y: Record<K, U>) {
    x = y;  // Error
    //~^ ERROR: Type 'Record<K, U>' is not assignable to type '{ [key: string]: T }'.
    y = x;  // Error
    //~^ ERROR: Type '{ [key: string]: T }' is not assignable to type 'Record<K, U>'.
}

// Repro from #14548

type Dictionary = {
    [key: string]: string;
};

interface IBaseEntity {
    name: string;
    properties: Dictionary;
}

interface IEntity<T extends string> extends IBaseEntity {
    properties: Record<T, string>;
}
