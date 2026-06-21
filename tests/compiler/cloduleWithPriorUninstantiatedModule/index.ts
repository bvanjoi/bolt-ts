// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleWithPriorUninstantiatedModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Non-ambient & uninstantiated module.
namespace Moclodule {
    export interface Someinterface {
        foo(): void;
    }
}

class Moclodule {
}

// Instantiated module.
namespace Moclodule {
    export class Manager {
    }
}