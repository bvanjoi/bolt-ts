// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/bestChoiceType.ts`, Apache-2.0 License

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