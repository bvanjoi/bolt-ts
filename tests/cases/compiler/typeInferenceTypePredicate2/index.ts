// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeInferenceTypePredicate2.ts`, Apache-2.0 License

[true, true, false, null]
    .filter((thing): thing is boolean => thing !== null)
    .map(thing => thing.toString());
