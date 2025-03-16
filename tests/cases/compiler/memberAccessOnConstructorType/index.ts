// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/memberAccessOnConstructorType.ts`, Apache-2.0 License

//@ run-fail

var f: new () => void;
f.arguments == 0;