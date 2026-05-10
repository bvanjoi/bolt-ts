// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplatesWithIncompleteNoSubstitutionTemplate1.ts`, Apache-2.0 License

//@compiler-options: target=es6

function f(x: TemplateStringsArray, y: string, z: string) {
}

// Incomplete call, not enough parameters.
f `123qdawdrqw //~ERROR: Unterminated template literal.