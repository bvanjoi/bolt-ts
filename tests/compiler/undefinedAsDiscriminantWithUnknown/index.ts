// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/undefinedAsDiscriminantWithUnknown.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@[strictNullChecks=true]  compiler-options: strictNullChecks
//@[strictNullChecks=false] compiler-options: strictNullChecks=false
//@run-fail

type S = 
| { type: 'string', value: string } 
| { type: 'number', value: number } 
| { type: 'unknown', value: unknown }
| { value: undefined };

declare var s: S

if (s.value !== undefined) {
  s;
}
else {
  s;
}