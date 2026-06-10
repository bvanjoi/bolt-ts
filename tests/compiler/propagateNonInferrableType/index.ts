// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propagateNonInferrableType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

declare function resolver<T>(): () => void;
declare function wrapResolver<T>(resolverFunction: () => void): void;

wrapResolver(resolver() || []);
