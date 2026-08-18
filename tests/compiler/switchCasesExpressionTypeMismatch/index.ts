// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchCasesExpressionTypeMismatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo { }

switch (0) {
    case Foo: break;    // Error
    //~^ ERROR: Type 'typeof Foo' is not comparable to type '0'.
    case "sss": break;  // Error
    //~^ ERROR: Type '"sss"' is not comparable to type '0'.
    case 123: break;    // Error
    //~^ ERROR: Type '123' is not comparable to type '0'.
    case true: break;   // Error
    //~^ ERROR: Type 'true' is not comparable to type '0'.
}

declare var q: string
declare var r: number | "hello"

switch (r) {
    case q: break
    case 42: break
    case true: break // Error
    //~^ ERROR: Type 'true' is not comparable to type 'number | "hello"'.
    case "hello": break
    case "world": break // Error
    //~^ ERROR: Type '"world"' is not comparable to type 'number | "hello"'.
}

var s: any = 0;

// No error for all
switch (s) {
    case Foo: break;
    case "sss": break;
    case 123: break;
    case true: break;
}
