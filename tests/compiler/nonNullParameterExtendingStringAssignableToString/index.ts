// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullParameterExtendingStringAssignableToString.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare function foo(p: string): void;

function fn<T extends string | undefined, U extends string>(one: T, two: U) {
    let three = Boolean() ? one : two;
    foo(one!);
    foo(two!);
    foo(three!); // this line is the important one
}