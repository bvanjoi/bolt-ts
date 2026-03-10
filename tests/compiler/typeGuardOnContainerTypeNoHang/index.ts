// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardOnContainerTypeNoHang.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export namespace TypeGuards {
    export function IsObject(value: any) : value is {[index:string]:any} {
        return typeof(value) === 'object'
    }

}
