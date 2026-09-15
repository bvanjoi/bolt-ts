// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUsedBeforeDefinedErrorInTypeContext.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/8775

interface IThing<T> {
    owner: T;
}

var foo = {
    one: {} as IThing<typeof foo>,
}

let baz = {
    two: {} as IThing<typeof bar>,
}

let bar = {
    three: {} as IThing<typeof bar>,
}

const qwe = {
    four: {} as IThing<typeof qwe>,
}
