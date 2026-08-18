// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplatesWithIncompleteTemplateExpressions2.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@skip-message-match

function f(x: TemplateStringsArray, y: string, z: string) {
}

// Incomplete call, enough parameters.
f `123qdawdrqw${ }${