// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedByDiscriminableUnion.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noImplicitAny

// repro #47682

type ADT = {
    kind: "a",
    method(x: string): number;
} | {
    kind: "b",
    method(x: number): string;
};


function invoke(item: ADT) {
    if (item.kind === "a") {
        item.method("");
    }
    else {
        item.method(42);
    }
}

invoke({
    kind: "a",
    method(a) {
        return +a;
    }
});

const kind = "a"
invoke({
    kind,
    method(a) {
        return +a;
    }
})
