// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/duplicateOverloadInTypeAugmentation1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
interface Array<T> {
    reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T,
        initialValue?: T): T;
    reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U,
        initialValue: U): U;
}
var a: Array<string>;
var r5 = a.reduce((x, y) => x + y);
//~^ ERROR: Variable 'a' is used before being assigned.