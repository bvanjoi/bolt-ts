// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializeVarArgs1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface Observable<T>{ }

 

interface ObservableArray<T> extends Observable<T[]>

{

    push(...values: T[]);

}

 

function observableArray<T>(): ObservableArray<T> { return null;}

 

var a =  observableArray<string>();

a.push('Some Value');
