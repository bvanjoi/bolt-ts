// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionExcessPropertyCheckNoApparentPropTypeMismatchErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface IStringDictionary<V> {
	[name: string]: V;
}
interface INumberDictionary<V> {
	[idx: number]: V;
}

declare function forEach<T>(from: IStringDictionary<T> | INumberDictionary<T>, callback: (entry: { key: any; value: T; }, remove: () => void) => any);

let count = 0;
forEach({ toString: 123 }, () => count++);
