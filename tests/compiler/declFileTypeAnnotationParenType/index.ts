// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationParenType.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: declaration

class c {
    private p: string;
    //~^ ERROR: Property 'p' has no initializer and is not definitely assigned in the constructor.
}

var x: (() => c)[] = [() => new c()];
var y = [() => new c()];

var k: (() => c) | string = (() => new c()) || "";
//~^ ERROR: This kind of expression is always truthy.
var l = (() => new c()) || "";
//~^ ERROR: This kind of expression is always truthy.