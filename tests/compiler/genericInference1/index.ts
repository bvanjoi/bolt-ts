// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericInference1.ts`, Apache-2.0 License

['a', 'b', 'c'].map(x => x.length);

let b: number[] = ['a', 'b', 'c'].map(x => x.length);
