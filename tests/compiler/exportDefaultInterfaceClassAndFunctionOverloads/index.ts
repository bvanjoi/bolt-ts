// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultInterfaceClassAndFunctionOverloads.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015

export default function foo(value: number): number
export default function foo(value: string): string
export default function foo(value: string | number): string | number {
    return 1
}
declare class Foo {}
export default Foo
//~^ ERROR: A module cannot have multiple default exports.
export default interface Bar {}
