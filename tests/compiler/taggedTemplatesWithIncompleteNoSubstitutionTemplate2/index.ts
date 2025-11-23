// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/taggedTemplatesWithIncompleteNoSubstitutionTemplate2.ts`, Apache-2.0 License

//@compiler-options: target=es6

function f(x: TemplateStringsArray, y: string, z: string) {
}

// Incomplete call, not enough parameters, at EOF.
f ` //~ERROR: Unterminated template literal.