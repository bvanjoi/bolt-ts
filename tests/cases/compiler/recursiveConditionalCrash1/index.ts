// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveConditionalCrash1.ts`, Apache-2.0 License

type C1<T> = [T extends string ? C1<T> : never][0];
type C2<T> = [T extends string ? [C2<T>] : never][0];
