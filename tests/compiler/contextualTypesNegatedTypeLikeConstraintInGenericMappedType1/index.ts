// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypesNegatedTypeLikeConstraintInGenericMappedType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type IntrinsicElements = {
  div: {
    onChange: (ev: Event) => void;
  };
  span: {
    onChange: (ev: Event) => void;
  };
};

type ElementType = keyof IntrinsicElements;

let DEFAULT_TABS_TAG = "div" as const;

type Props<TTag extends ElementType, Overrides = {}> = Omit<
  IntrinsicElements[TTag],
  keyof Overrides
> &
  Overrides;

type TabGroupProps<TTag extends ElementType = typeof DEFAULT_TABS_TAG> = Props<
  TTag,
  {
    defaultIndex?: number;
    onChange?: (index: number) => void;
    selectedIndex?: number;
    vertical?: boolean;
    manual?: boolean;
  }
>;

interface _internal_ComponentTabGroup {
  <TTag extends ElementType = typeof DEFAULT_TABS_TAG>(
    props: TabGroupProps<TTag>,
  ): null;
}

declare let TabGroup: _internal_ComponentTabGroup;

TabGroup({
  defaultIndex: 0,
  onChange: (index) => {
    const i: number = index;
  },
});
