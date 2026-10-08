// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypeDeepDeclarationEmit.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail

export type Validator<T> = NativeTypeValidator<T> | ObjectValidator<T>

export type NativeTypeValidator<T> = (n: any) => T | undefined
export type ObjectValidator<O> = {
  [K in keyof O]: Validator<O[K]> 
}

//native validators
export declare const SimpleStringValidator: NativeTypeValidator<string>;

///object validator function
export declare const ObjValidator: <V>(validatorObj: ObjectValidator<V>) => (o: any) => V;

export const test  = {
  Test: {
    Test1: {
      Test2: SimpleStringValidator
    },
  }
}

export const validatorFunc = ObjValidator(test);
export const outputExample = validatorFunc({
  Test: {
    Test1: {
      Test2: "hi"
    },
  }
});
