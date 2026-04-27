// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectLitPropertyScoping.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function makePoint(x: number, y: number) {
    return {
        get x() {
            return x;
        },
        get y() {
            return y;
        },
        dist: function () {
            return Math.sqrt(x * x + y * y);
        }
    }
};