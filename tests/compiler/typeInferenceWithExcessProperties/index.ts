// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInferenceWithExcessProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Named {
    name: string;
}

function parrot<T extends Named>(obj: T): T {
    return obj;
}


parrot({
    name: "TypeScript",
});

parrot({
    name: "TypeScript",
    age: 5,
});

parrot({
    name: "TypeScript",
    age: function () { },
});

parrot({
    name: "TypeScript",
    sayHello() {
    },
});
