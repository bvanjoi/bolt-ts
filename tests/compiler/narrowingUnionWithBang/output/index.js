// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingUnionWithBang.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var working = null;
if (working.thing.name !== 'Correct') {
  console.log(working.thing.message);
} else {
  console.log(working.thing.id);
}

var borked = null;
if (borked.thing.name !== 'Correct') {
  console.log(borked.thing.message);
} else {
  console.log(borked.thing.id);
}

var fixed = null;
if (fixed.thing.name !== 'Correct') {
  console.log(fixed.thing.message);
} else {
  console.log(fixed.thing.id);
}
