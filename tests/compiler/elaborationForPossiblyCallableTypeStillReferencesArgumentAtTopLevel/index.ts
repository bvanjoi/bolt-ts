// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/elaborationForPossiblyCallableTypeStillReferencesArgumentAtTopLevel.ts`, Apache-2.0 License

declare var ohno: new () => number;
declare function ff(t: number): void;
ff(ohno)
//~^ ERROR: Argument of type 'new () => number' is not assignable to parameter of type 'number'.