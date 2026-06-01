// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultInterfaceAndFunctionOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export default function foo(value: number): number
export default function foo(value: string): string
export default function foo(value: string | number): string | number {
    return 1
}
export default interface Foo {}
