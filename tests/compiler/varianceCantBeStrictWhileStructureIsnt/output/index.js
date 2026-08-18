// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/varianceCantBeStrictWhileStructureIsnt.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: strictFunctionTypes=false




a = b;
b = a;
a2 = b2;
b2 = a2;
a = b2;
b = a2;
a2 = b;
b2 = a;