// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/reexportMissingDefault3.ts`, Apache-2.0 License

export { b } from "./b";
export { default as a } from "./b";
//~^ ERROR: Module './b' has no exported member 'default'.