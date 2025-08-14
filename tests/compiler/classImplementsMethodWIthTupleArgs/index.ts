// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classImplementsMethodWIthTupleArgs.ts`, Apache-2.0 License

//@compiler-options: strict

declare class MySettable implements Settable {
    set(option: Record<string, unknown>): void;
    set(name: string, value: unknown): void;
}

interface Settable {
    set(...args: [option: Record<string, unknown>] | [name: string, value: unknown] | [name: string]): void;
}
