// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_writeOnly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f(x = 0, b = false) {
  //~^ ERROR: 'x' is declared but its value is never read.
    // None of these statements read from 'x', so it will be marked unused.
    x = 1;
    ([x] = [1]);
    ({ x } = { x: 1 });
    ({ x: x } = { x: 1 });
    ({ a: [{ b: x }] } = { a: [{ b: 1 }] });
    ({ x = 2 } = { x: b ? 1 : undefined });
    let used = 1;
    ({ x = used } = { x: b ? 1 : undefined });

    let y = 0;
    // This is a write access to y, but not a write-*only* access.
    f(y++);

    let z = 0;
    //~^ ERROR: 'z' is declared but its value is never read.
    f(z = 1); // This effectively doesn't use `z`, values just pass through it.
}
function f2(_: ReadonlyArray<number>): void {}
