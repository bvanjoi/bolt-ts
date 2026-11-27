
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emptyAnonymousObjectNarrowing.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@[strictNullChecks=true]  compiler-options: strictNullChecks=true
//@[strictNullChecks=false] compiler-options: strictNullChecks=false
//@run-fail
if (nonNull === 'foo') {
  nonNull;
} else {
  nonNull;
}


if (nonNull === obj) {
  nonNull;
} else {
  nonNull;
}

function f1(x) {
  if (nonNull === x) {
    nonNull;
  } else {
    nonNull;
  }
  
}
function f2(x) {
  if (nonNull === x) {
    nonNull;
  } else {
    nonNull;
  }
  
}

if (nonNull === union) {
  nonNull;
} else {
  nonNull;
}

if (nonNull === undefined) {
  nonNull;
} else {
  nonNull;
}

if (nonNull === null) {
  nonNull;
} else {
  nonNull;
}

if (nonNull == undefined) {
  nonNull;
} else {
  nonNull;
}

var foo = (value// Repro from #50567
) => {
  if (!value) {
    return 'foo';
  }
  
  if (value === 'xyz') {
    return value;// Type '{}' is not assignable to type 'string'.
    
  }
  
  return '';
};