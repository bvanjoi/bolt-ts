// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritSameNamePropertiesWithDifferentOptionality.ts`, Apache-2.0 License

const [, a = ''] = ''.match('') || [];

a.toFixed()
//~^ ERROR: Property 'toFixed' does not exist on type 'string'.