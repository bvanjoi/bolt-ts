// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceFromIncompleteSource.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface ListProps<T, K extends keyof T> {
  items: T[];
  itemKey: K;
  prop: number;
}

declare const Component: <T, K extends keyof T>(x: ListProps<T, K>) => void;

Component({items: [{name:' string'}], itemKey: 'name' });
//~^ ERROR: Property 'prop' is missing.
