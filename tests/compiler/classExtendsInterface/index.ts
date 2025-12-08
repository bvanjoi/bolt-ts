// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classExtendsInterface.ts`, Apache-2.0 License

interface Comparable {}
class A extends Comparable {}
//~^ ERROR: Cannot find name 'Comparable'.
class B implements Comparable {}

interface Comparable2<T> {}
class A2<T> extends Comparable2<T> {}
//~^ ERROR: Cannot find name 'Comparable2'.
class B2<T> implements Comparable2<T> {}
