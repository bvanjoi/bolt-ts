// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypeAppliedToVarArgs.ts`, Apache-2.0 License

function delegate(instance: any, method: (...args: any[]) => any, data?: any): (...args: any[]) => any {
    return function () { };
}

class Foo{


    Bar() {
        delegate(this, function (source, args2)
        {
            var a = source.node;
            var b = args2.node;
        } );
    }
}
