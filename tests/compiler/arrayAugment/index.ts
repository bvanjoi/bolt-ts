// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayAugment.ts`, Apache-2.0 License

//@ run-fail

interface Array<T> {
    split: (parts: number) => T[][];
}

var x = [''];
var y = x.split(4);
var y: string[][]; // Expect no error here
