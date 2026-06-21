// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleWithPriorInstantiatedModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Non-ambient & instantiated module.
namespace Moclodule {
  //~^ ERROR: A namespace declaration cannot be located prior to a class or function with which it is merged.
    export interface Someinterface {
        foo(): void;
    }
    var x = 10;
}

class Moclodule {
}

// Instantiated module.
namespace Moclodule {
    export class Manager {
    }
}
