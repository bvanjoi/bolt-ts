// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalTypeContextualTypeSimplificationsSuceeds.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface Props {
    when: (value: string) => boolean;
}

function bad<P extends Props>(
    attrs: string extends keyof P ? { [K in keyof P]: P[K] } : { [K in keyof P]: P[K] }) { }
function good1<P extends Props>(
    attrs: string extends keyof P ? P : { [K in keyof P]: P[K] }) { }
function good2<P extends Props>(
    attrs: { [K in keyof P]: P[K] }) { }

bad({ when: value => false });
good1({ when: value => false });
good2({ when: value => false });