// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritedConstructorPropertyContextualType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail
interface State {
    version: 2
}
declare class Base<S> {
    state: S
}
class Assignment extends Base<State> {
    constructor() {
        super()
        this.state = { version: 2 }
    }
}