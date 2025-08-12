// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateTypeParameters3.ts`, Apache-2.0 License

interface X {
x: () => <A, A>() => void;
//~^ ERROR: Duplicate identifier 'A'.
}
 
