// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]
//@compiler-options: noUnusedLocals

declare var console: { log(msg: any): void; }
class Animal {
    constructor(private species: string) {
    }

    printSpecies() {
        let { species } = this;
        console.log(species);
    }
}

