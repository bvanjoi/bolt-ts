// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/objectLiteralWithGetAccessorInsideFunction.ts`, Apache-2.0 License

function bar() {
    var x = {
        get _extraOccluded() {
            var occluded = 0;
            return occluded;
        },
    }
}