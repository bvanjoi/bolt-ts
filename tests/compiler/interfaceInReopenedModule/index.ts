// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interfaceInReopenedModule.ts`, Apache-2.0 License

module m {
}

// In second instance of same module, exported interface is not visible
module m {
    interface f {}
    export class n { 
        private n: f;
    }
}
