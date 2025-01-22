// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericInference1.ts`, Apache-2.0 License

['a', 'b', 'c'].map(x => x.length);

let b: number[] = ['a', 'b', 'c'].map(x => x.length);
