// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noAsConstNameLookup.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Store = { a: 123 }
export type Cleaner = <W extends Store>(runner: FeatureRunner<W>) => Promise<any>

export class FeatureRunner<W extends Store> {
    private readonly cleaners: Cleaner[] = []

    async runFeature(): Promise<any> {
        const objectWhichShouldBeConst = {
            flags: {},
            settings: {},
        } as const;
        return objectWhichShouldBeConst
    }

    async run(): Promise<any> {
        const result = {}
        this.cleaners.forEach(c => c(this))
        return result
    }
}

export class C<T> {
    f(): void {
        let one = 1 as const;
    }
}
new C<string>().f();
