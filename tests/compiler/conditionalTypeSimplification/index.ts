// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/conditionalTypeSimplification.ts`, Apache-2.0 License

interface AbstractSchema<S, V> {
  m1<T> (v: T): SchemaType<S, Exclude<V, T>>;
  m2<T> (v: T): SchemaType<S, T>;
}

type SchemaType<S, V> = S extends object ? AnySchema<V> : never;
interface AnySchema<V> extends AnySchemaType<AnySchema<undefined>, V> { }
interface AnySchemaType<S extends AbstractSchema<any, any>, V> extends AbstractSchema<S, V> { }
