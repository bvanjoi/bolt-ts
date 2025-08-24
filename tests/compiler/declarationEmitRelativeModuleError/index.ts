// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/declarationEmitRelativeModuleError.ts`, Apache-2.0 License

declare module "b:block" { // <-- no error anymore
    
}

declare module "b:/block" { // <-- still an error
//~^ ERROR: Ambient module declaration cannot specify relative module name.    
}