// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ambientExternalModuleWithRelativeModuleName.ts`, Apache-2.0 License

declare module "./relativeModule" {
//~^ ERROR: Ambient module declaration cannot specify relative module name.
  var x: string;
}

declare module ".\\relativeModule" {
//~^ ERROR: Ambient module declaration cannot specify relative module name.
  var x: string;
}