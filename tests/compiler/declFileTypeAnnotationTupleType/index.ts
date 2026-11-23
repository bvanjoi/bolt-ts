// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationTupleType.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=ES5
//@[target=ES2015]  compiler-options: target=ES2015
//@compiler-options: declaration

class c {
}
namespace m {
    export class c {
    }
    export class g<T> {
    }
}
class g<T> {
}

// Just the name
var k: [c, m.c] = [new c(), new m.c()];
var l = k;

var x: [g<string>, m.g<number>, () => c] = [new g<string>(), new m.g<number>(), () => new c()];
var y = x;