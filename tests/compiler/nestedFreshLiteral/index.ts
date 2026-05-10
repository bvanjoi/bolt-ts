// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedFreshLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

interface CSSProps  {
  color?: string
}
interface NestedCSSProps {
  nested?: NestedSelector
}
interface NestedSelector  {
  prop: CSSProps;
}

let stylen: NestedCSSProps = {
  nested: { prop: { colour: 'red' } }
  //~^ ERROR: Object literal may only specify known properties, and 'colour' does not exist in type 'CSSProps'.
}