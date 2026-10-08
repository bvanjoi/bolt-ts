// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/trivialSubtypeReductionNoStructuralCheck.ts`, Apache-2.0 License

//@compiler-options: strict
//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015

declare const props: WizardStepProps;
export class Wizard {
  get steps() {
    //~^ ERROR: 'steps' implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions.
    return {
      wizard: this,
      ...props,
    } as WizardStepProps;
  }
}

export interface WizardStepProps {
  wizard?: Wizard;
}