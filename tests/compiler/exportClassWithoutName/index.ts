// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exportClassWithoutName.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015

export class {  //~ERROR: A class declaration without the 'default' modifier must have a name.
}