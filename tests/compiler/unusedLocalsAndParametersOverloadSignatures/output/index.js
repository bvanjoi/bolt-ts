// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndParametersOverloadSignatures.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters
export function func(details, message) {
  return details + message;
}
export class C {
  constructor(details, message) {details + message;}
  method(details, message) {
    return details + message;
  }
}
export function genericFunc(details, message) {
  return details + message;
}