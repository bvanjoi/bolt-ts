// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamInOverride.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Z {
    public func(): void { }
}
class Y extends Z {
    public func(value?: any): void { }
}
