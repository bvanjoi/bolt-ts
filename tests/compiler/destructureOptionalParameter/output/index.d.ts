declare function f1({a, b}?: {
  a: number;
  b: string;
}): void;
declare function f2({a, b}: {
  a: number;
  b: number;
}): void;
interface Type {
  t: void;
}
interface QueryMetadata {
  q: void;
}
interface QueryMetadataFactory {
  (selector: Type | string, {descendants, read}?: {
    descendants?: boolean;
    read?: any;
  }): ParameterDecorator;
  new (selector: Type | string, {descendants, read}?: {
    descendants?: boolean;
    read?: any;
  }): QueryMetadata;
}
