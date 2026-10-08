// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorConstructorSubtypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5,dom]

// In Node, ErrorConstructor is augmented with extra properties. Excerpted below.
interface ErrorConstructor {
  captureStackTrace(targetObject: Object, constructorOpt?: Function): void;
}

declare var x: ErrorConstructor
x = Error; // OK
x = RangeError;
new x().message
x.captureStackTrace
