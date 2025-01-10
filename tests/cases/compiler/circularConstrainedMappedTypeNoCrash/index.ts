// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/circularConstrainedMappedTypeNoCrash.ts`, Apache-2.0 License

type Loop<T, U extends Loop<T, U>> = {
  [P in keyof T]: U[P] extends boolean ? number : string;
};