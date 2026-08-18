// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericCapturingFunctionNarrowing.ts`, Apache-2.0 License
function needsToNarrowTheType(thing) {
  if (hasAFoo(thing)) {
    console.log(thing.foo);
  } else {
    console.log(thing.bar);
  }
  
  function hasAFoo(value) {
    return 'foo' in value;
  }
}