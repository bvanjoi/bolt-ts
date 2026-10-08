// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedGetterInClass.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class Employee {
    private _fullName: string;
    //~^ ERROR: Property '_fullName' has no initializer and is not definitely assigned in the constructor.

    private get fullName(): string {
      //~^ ERROR: 'fullName' is declared but its value is never read.
        return this._fullName;
    }
    // Will not also error on the setter
    private set fullName(_: string) {}
}
