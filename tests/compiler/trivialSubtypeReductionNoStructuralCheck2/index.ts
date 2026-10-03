// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/trivialSubtypeReductionNoStructuralCheck2.ts`, Apache-2.0 License

//@compiler-options: strict
//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015

declare const props: WizardStepProps;
export class Wizard {
  get steps() {
    return {
      wizard: this as Wizard,
      ...props,
    } as WizardStepProps;
  }
}

export interface WizardStepProps {
  wizard?: Wizard;
}