// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardNarrowByUntypedField.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6]
//@run-fail

declare function hasOwnProperty<P extends PropertyKey>(target: {}, property: P): target is { readonly [K in P]: unknown };
declare const arrayLikeOrIterable: ArrayLike<any> | Iterable<any>;
if (hasOwnProperty(arrayLikeOrIterable, 'length')) {
    let x: number = arrayLikeOrIterable.length;
}