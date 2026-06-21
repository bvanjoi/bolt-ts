// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypeBasedOnIntersectionWithAnyInTheMix2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type IntrinsicElements = {
  a: {
    href?: string;
  };
  div: {
    dir?: string;
  };
};

type Component<Props> = (props: Props) => unknown;

interface NestedMDXComponents {
  [key: string]: Component<any>;
}

type MDXComponents = NestedMDXComponents & {
  [Key in keyof IntrinsicElements]?: Component<IntrinsicElements[Key]>;
};

export interface MDXProps {
  components?: MDXComponents;
}

declare function MyMDXComponent(props: MDXProps): null;

MyMDXComponent({
  components: {
    a(props) {
      return null;
    },
    div(props) {
      return null;
    },
  },
});
