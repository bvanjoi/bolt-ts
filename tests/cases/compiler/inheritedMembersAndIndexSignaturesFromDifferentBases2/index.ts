// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inheritedMembersAndIndexSignaturesFromDifferentBases2.ts`, Apache-2.0 License

interface A<T> {
  [n: number]: T;
}

interface B {
  foo: number;
}

interface C extends B, A<string> { } // Should succeed