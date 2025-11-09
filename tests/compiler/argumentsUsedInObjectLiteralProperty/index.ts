// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsUsedInObjectLiteralProperty.ts`, Apache-2.0 License

class A {
    public static createSelectableViewModel(initialState?: any, selectedValue?: any) {
        return {
            selectedValue: arguments.length
        };
    }
}