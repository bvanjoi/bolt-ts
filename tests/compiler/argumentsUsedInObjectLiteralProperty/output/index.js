// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsUsedInObjectLiteralProperty.ts`, Apache-2.0 License
class A {
  static createSelectableViewModel(initialState, selectedValue) {
    return {
          selectedValue: arguments.length      
    };
  }
}