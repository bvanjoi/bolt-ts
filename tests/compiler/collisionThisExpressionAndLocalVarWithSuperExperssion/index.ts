// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndLocalVarWithSuperExperssion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class a {
    public foo() {
    }
}
class b extends a {
    public foo() {
        var _this = 10;
        var f = () => super.foo();
    }
}
class b2 extends a {
    public foo() {
        var f = () => {
            var _this = 10;
            return super.foo()
        }
    }
}