// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericWithCallSignatureReturningSpecialization.ts`, Apache-2.0 License

interface B<T> {
  f(): B<number>;
  (value: T): void;
}
var x: B<boolean>;
x(true); // was error
let b: B<string> = x.f();
//~^ ERROR: Type 'B<number>' is not assignable to type 'B<string>'.