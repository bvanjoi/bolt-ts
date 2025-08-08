// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/circularTypeofWithFunctionModule.ts`, Apache-2.0 License

class Foo {}

function maker (value: string): typeof maker.Bar {
    return maker.Bar;
}

namespace maker {
    export class Bar extends Foo {}
}
