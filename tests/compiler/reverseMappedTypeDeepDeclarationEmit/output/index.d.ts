type Validator<T> = NativeTypeValidator<T> | ObjectValidator<T>;
type NativeTypeValidator<T> = (n: any) => T | undefined;
type ObjectValidator<O> = {
[K in keyof O]: Validator<O[K]>
};
export var SimpleStringValidator: NativeTypeValidator<string>;
export var ObjValidator: <V>(validatorObj: ObjectValidator<V>) => (o: any) => V;
export var test: { Test: { Test1: { Test2: NativeTypeValidator<string>; }; }; };
export var validatorFunc: (o: any) => ObjectValidator<V>;
export var outputExample: ObjectValidator<V>;
