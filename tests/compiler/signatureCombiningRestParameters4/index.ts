// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureCombiningRestParameters4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare class Node<Options = any> {
  type: string;
  name: string;
  parent: Node | null;
  child: Node | null;
  options: Options;
}

interface NodeConfig<Options = any> {
  extendMarkSchema?:
    | ((
        this: {
          name: string;
          options: Options;
        },
        extension: Node,
      ) => Record<string, any>)
    | null;
}

declare class Mark<Options = any> {
  options: Options;
  config: MarkConfig;
}

interface MarkConfig<Options = any> {
  extendMarkSchema?:
    | ((
        this: {
          name: string;
          options: Options;
        },
        extension: Mark,
      ) => Record<string, any>)
    | null;
}

type AnyConfig = NodeConfig | MarkConfig;
type AnyExtension = Node | Mark;

declare const e: AnyExtension;

type RemoveThis<T> = T extends (...args: any) => any
  ? (...args: Parameters<T>) => ReturnType<T>
  : T;

declare function getExtensionField<T = any>(
  extension: AnyExtension,
  field: string,
): RemoveThis<T>;

const extendMarkSchema = getExtensionField<AnyConfig["extendMarkSchema"]>(
  e,
  "extendMarkSchema",
);

declare const extension: Mark<any>;

if (extendMarkSchema) {
  extendMarkSchema(extension); // error
  //~^ ERROR: Type 'Mark<any>' is missing the following properties from type 'Node<any>': type, name, and 2 more.
}

export {};
