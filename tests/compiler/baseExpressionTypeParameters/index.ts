// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/baseExpressionTypeParameters.ts`, Apache-2.0 License

function base<T>() {
    class Base {
        static prop: T;
    }
    return Base;
}

class Gen<T> extends base<T>() {}  // Error, T not in scope
//~^ ERROR: Cannot find name 'T'.
class Spec extends Gen<string> {}

<string>Spec.prop;