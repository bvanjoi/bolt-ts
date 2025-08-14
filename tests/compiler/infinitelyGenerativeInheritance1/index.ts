// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyGenerativeInheritance1.ts`, Apache-2.0 License

interface Stack<T> {
  pop(): T
  zip<S>(a: Stack<S>): Stack<{ x: T; y: S }>
}

interface MyStack<T> extends Stack<T> {
  zip<S>(a: Stack<S>): Stack<{ x: T; y: S }>
}
