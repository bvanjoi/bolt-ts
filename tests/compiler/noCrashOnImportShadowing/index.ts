// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCrashOnImportShadowing.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

import { B } from "./a";

const x: B = { x: "" };
B.zzz;

import * as OriginalB from "./b";
OriginalB.zzz;

const y: OriginalB = x;
//~^ ERROR: Cannot find name 'OriginalB'.