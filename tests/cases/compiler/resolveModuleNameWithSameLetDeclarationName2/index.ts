// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/resolveModuleNameWithSameLetDeclarationName2.ts`, Apache-2.0 License

declare module "punycode" {
  interface ucs2 {
      decode(string: string): string;
      encode(codePoints: number[]): string;
  }

  export let ucs2: ucs2;
}