// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterAsBaseClass.ts`, Apache-2.0 License

class C<T> extends T {}
//~^ ERROR: Cannot find name 'T'.
class C2<T> implements T {}
//~^ ERROR: A class can only implement an object type or intersection of object types with statically known members.
class C3<T, U> implements T, U {}
//~^ ERROR: A class can only implement an object type or intersection of object types with statically known members.
//~| ERROR: A class can only implement an object type or intersection of object types with statically known members.
