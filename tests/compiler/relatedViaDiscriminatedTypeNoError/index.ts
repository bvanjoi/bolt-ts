// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/relatedViaDiscriminatedTypeNoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Model {
    constructor(public flag: boolean) {}
}

type DiscriminatedUnion = { flag: true } | { flag: false };
class A<T extends DiscriminatedUnion> {
    constructor(public model: T) { }
}

class B extends A<Model> { }
