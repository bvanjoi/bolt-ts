// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/arrayConstructors1.ts`, Apache-2.0 License

var x: string[];
x = new Array(1);
x = new Array('hi', 'bye'); 
x = new Array<string>('hi', 'bye');

var y: number[];
y = new Array(1);
y = new Array(1,2);
y = new Array<number>(1, 2);