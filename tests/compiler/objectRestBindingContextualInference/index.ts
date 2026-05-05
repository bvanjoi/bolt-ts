// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectRestBindingContextualInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type ImageHolder<K extends string> = {
  [P in K]: string;
};

type SetupImageRefs<K extends string> = {
  [P in K]: File;
};

type SetupImages<K extends string> = SetupImageRefs<K> & {
  prepare: () => { type: K };
};

interface TestInterface {
  name: string;
  image: string;
}

declare function setupImages<R extends ImageHolder<K>, K extends string>(
  item: R,
  keys: K[]
): SetupImages<K>;

declare const test: TestInterface;

const { prepare, ...rest } = setupImages(test, ["image"]);

setupImages(test, ["image"]);