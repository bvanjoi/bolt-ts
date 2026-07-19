// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericCallbacksAndClassHierarchy.ts`, Apache-2.0 License

module M {
  export interface I<T> {
      subscribe(callback: (newValue: T) => void ): any;
  }
  export class C1<T> {
      public value: I<T>;
      //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
  }
  export class A<T> {
      public dummy: any;
  }
  export class B<T> extends C1<A<T>> { }
  export class D<T> {
      _subscribe(viewModel: B<T>): void {
          var f = (newValue: A<T>) => { };

          var v: I<A<T>> = viewModel.value;

          // both of these should work
          v.subscribe(f);
          v.subscribe((newValue: A<T>) => { });
      }
  }
}
