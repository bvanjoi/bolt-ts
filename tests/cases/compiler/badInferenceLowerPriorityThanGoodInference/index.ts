// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/badInferenceLowerPriorityThanGoodInference.ts`, Apache-2.0 License

//@ run-fail

interface Foo<A> {
  a: A;
  b: (x: A) => void;
}

declare function canYouInferThis<A>(fn: () => Foo<A>): A;

const result = canYouInferThis(() => ({
  a: { BLAH: 33 },
  b: x => { }
}))

result.BLAH;

// Repro from #26629

function goofus <ARGS extends any[]> (f: (...args: ARGS) => any ) {}

goofus((a: string) => ({ dog() { return a; } }));
goofus((a: string) => ({ dog: function() { return a; } }));
