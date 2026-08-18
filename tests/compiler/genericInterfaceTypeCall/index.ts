// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericInterfaceTypeCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface Foo<T> {
    reject(arg: T): void;
}
var foo: Foo<string>
 
interface bar<T> {
    fail(func: (arg: T) => void ): void;
    fail2(func2: { (arg: T): void; }): void;
}
var test: bar<string>;
 
test.fail(arg => foo.reject(arg));
//~^ ERROR: Variable 'test' is used before being assigned.
test.fail2(arg => foo.reject(arg)); // Error: Supplied parameters do not match any signature of call target
//~^ ERROR: Variable 'test' is used before being assigned.
