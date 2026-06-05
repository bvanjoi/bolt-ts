// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterUnderscoreIUsage.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

declare var console: { log(msg?: string): void; };
var _i = "This is what I'd expect to see";
class Foo {
    constructor(...args: any[]) {
        console.log(_i); // This should result in error
    }
}
new Foo();
