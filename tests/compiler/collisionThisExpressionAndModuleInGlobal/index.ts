// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/collisionThisExpressionAndModuleInGlobal.ts`, Apache-2.0 License

namespace _this { //Error
    class c {
    }
}
var f = () => this;