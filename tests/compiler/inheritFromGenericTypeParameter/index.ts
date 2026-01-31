// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritFromGenericTypeParameter.ts`, Apache-2.0 License

class C<T> extends T { }
//~^ ERROR: Cannot find name 'T'.
interface I<T> extends T { }
//~^ ERROR: An interface can only extend an object type or intersection of object types with statically known members.