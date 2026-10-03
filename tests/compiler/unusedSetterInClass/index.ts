// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedSetterInClass.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class Employee {
    private _fullName: string;
    //~^ ERROR: Property '_fullName' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: '_fullName' is declared but its value is never read.

    private set fullName(newName: string) {
    //~^ ERROR: 'fullName' is declared but its value is never read.
        this._fullName = newName;
    }
}