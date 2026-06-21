// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericInferenceDefaultTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

type Type = {
    a: (e: string) => void;
    b: (e: number) => void;
}

declare function f1<T extends keyof Type = "a">(props: Type[T]): void;

f1(event => { });
f1<"a">(event => { });
f1<"b">(event => { });
