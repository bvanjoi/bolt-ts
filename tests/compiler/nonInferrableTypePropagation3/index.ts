// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonInferrableTypePropagation3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare type Callback<Args extends any[], Out, R> = (...args: Args) => (data: Out) => R;
declare function factory<Out>(): <Args extends any[], R>(callback: Callback<Args, Out, R>) => (...args: Args) => R;

const make = factory<{id: string, age: number}[]>();

const usersOverAge = make((age: number) => data => {
    return data.filter(user => user.age >= age);
});
