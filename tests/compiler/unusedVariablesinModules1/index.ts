// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesinModules1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

export {};

var x: string;
//~^ ERROR: 'x' is declared but its value is never read.

export var y: string;

declare const z: number;

declare class A {
  m(): void
  n: number
}
