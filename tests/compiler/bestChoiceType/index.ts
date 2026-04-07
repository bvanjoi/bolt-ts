// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/bestChoiceType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

(''.match(/ /) || []).map(s => s.toLowerCase());

// Similar cases

function f1() {
    let x = ''.match(/ /);
    let y = x || [];
    let z = y.map(s => s.toLowerCase());
}

function f2() {
    let x = ''.match(/ /);
    let y = x ? x : [];
    let z = y.map(s => s.toLowerCase());
}