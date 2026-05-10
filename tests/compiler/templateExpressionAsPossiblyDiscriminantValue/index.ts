// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateExpressionAsPossiblyDiscriminantValue.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit


type BiomePlainLinkProps = {
  href: string;
  onClick?: (event: string) => void;
}

type BiomeButtonProps = {
  href?: never;
  onClick?: (event: number) => void;
}

export type ClickableDiscriminatedUnion =
  | BiomePlainLinkProps
  | BiomeButtonProps;

const p3: ClickableDiscriminatedUnion = {
  href: `2${undefined}332132`,
  onClick: (ev) => console.log('@@@@', ev),
}

const p4: ClickableDiscriminatedUnion = {
  href: `2${undefined}332132`,
  onClick: (ev) => {
    const a: number = ev;
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
    console.log('@@@@', ev)
  },
}
