// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularBaseConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type A<T> = T;

type B<T> = T extends any[]
    ? never
    : A<T> extends infer key
    ? key extends keyof T
        ? B<T[key]>
        : never
    : never;

function foo<T>() {
    `${a}` as B<T>;
    //~^ ERROR: Cannot find name 'a'.
}
