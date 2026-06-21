// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidUseOfTypeAsNamespace.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface OhNo {
}

declare let y: OhNo.hello;
//~^ ERROR: Cannot find name 'OhNo'.