// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multipleInferenceContexts.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

type ConstructorOptions<Data> =
    & ComponentOptionsProperties<Data>
    & ThisType<Instance<Data>>;

interface ComponentOptionsProperties<Data> {
    data: Data;
    render(): unknown;
}

interface Instance<Data> {
    get<K extends keyof Data>(name: K): unknown;
}

declare var Moon: {
    <Data>(options?: ConstructorOptions<Data>): Instance<Data>;
};

const r2 = Moon({
    data: { msg: "" },
    render() {
        const h = (x: unknown) => x;
        return h(this.get("msg"));
    },
});

