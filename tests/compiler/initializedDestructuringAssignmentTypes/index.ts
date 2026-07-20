// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/initializedDestructuringAssignmentTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const [, a = ''] = ''.match('') || [];

a.toFixed()
//~^ ERROR: Property 'toFixed' does not exist on type 'string'.