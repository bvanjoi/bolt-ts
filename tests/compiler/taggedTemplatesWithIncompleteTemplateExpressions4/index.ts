// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplatesWithIncompleteTemplateExpressions4.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@skip-message-match

function f(x: TemplateStringsArray, y: string, z: string) {
}

// Incomplete call, but too many parameters.
f `123qdawdrqw${ 1 }${ }${ 