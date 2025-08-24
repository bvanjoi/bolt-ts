// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticInstanceResolution2.ts`, Apache-2.0 License

class A { }
A.hasOwnProperty('foo');

class B {
    constructor() { }
}
B.hasOwnProperty('foo');

let a: boolean = A.hasOwnProperty('foo');
let b: boolean = B.hasOwnProperty('foo');