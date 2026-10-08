// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationUnionType.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: declaration

class c {
    private p: string;
    //~^ ERROR: Property 'p' has no initializer and is not definitely assigned in the constructor.
}
namespace m {
    export class c {
        private q: string;
    //~^ ERROR: Property 'q' has no initializer and is not definitely assigned in the constructor.
    }
    export class g<T> {
        private r: string;
    //~^ ERROR: Property 'r' has no initializer and is not definitely assigned in the constructor.
    }
}
class g<T> {
    private s: string;
    //~^ ERROR: Property 's' has no initializer and is not definitely assigned in the constructor.
}

// Just the name
var k: c | m.c = new c() || new m.c();
var l = new c() || new m.c();

var x: g<string> | m.g<number> |  (() => c) = new g<string>() ||  new m.g<number>() || (() => new c());
var y = new g<string>() || new m.g<number>() || (() => new c());