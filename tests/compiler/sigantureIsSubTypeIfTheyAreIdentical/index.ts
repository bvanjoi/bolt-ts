// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/sigantureIsSubTypeIfTheyAreIdentical.ts`, Apache-2.0 License

//@compiler-options: target=es2015


interface ICache {
    get<T>(key: string): T;
}
class CacheService implements ICache { // Should not error that property type of get are incomaptible
    get<T>(key: string): T {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type 'T'.
    }
}