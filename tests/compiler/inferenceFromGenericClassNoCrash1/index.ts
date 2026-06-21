// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceFromGenericClassNoCrash1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/61633#issuecomment-2841778576

function RenderFlagsMixin<T extends new (...args: any[]) => object>(Base?: T) {}
class Container<T> {
  t: T;
  //~^ ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.
}
RenderFlagsMixin(Container);
