// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interfaceSubtyping.ts`, Apache-2.0 License

interface iface {
    foo(): void;
}
class Camera implements iface{
    constructor (public str: string) {
    }
    foo() {  return "s";   }
}

