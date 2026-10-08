// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/methodContainingLocalFunction.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// The first case here (BugExhibition<T>) caused a crash. Try with different permutations of features.
class BugExhibition<T> {
    public exhibitBug() {
        function localFunction() { }
        var x: { (): void; };
        x = localFunction;
    }
}

class BugExhibition2<T> {
    private static get exhibitBug() {
        function localFunction() { }
        var x: { (): void; };
        x = localFunction;
        return null;
    }
}

class BugExhibition3<T> {
    public exhibitBug() {
        function localGenericFunction<U>(u?: U) { }
        var x: { (): void; };
        x = localGenericFunction;
    }
}

class C {
    exhibit() {
        var funcExpr = <U>(u?: U) => { };
        var x: { (): void; };
        x = funcExpr;
    }
}

namespace M {
    export function exhibitBug() {
        function localFunction() { }
        var x: { (): void; };
        x = localFunction;
    }
}

enum E {
    A = (() => {
        function localFunction() { }
        var x: { (): void; };
        x = localFunction;
        return 0;
    })()
}