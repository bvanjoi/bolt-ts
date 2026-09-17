// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modifierParenCast.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

let readonly: any = undefined;
let override: any = undefined;
let out: any = undefined;
let declare: any = undefined;

export const a = (readonly as number);
export const b = (override as number);
export const c = (out as number);
export const d = (declare as number);