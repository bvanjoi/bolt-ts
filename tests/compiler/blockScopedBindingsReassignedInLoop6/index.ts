// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedBindingsReassignedInLoop6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f1() {
    for (let [x, y] = [1, 2]; x < y; ++x, --y) {
        let a = () => x++ + y++;
        if (x == 1)
            break;
        else if (y == 2)
            y = 5;
        else
            return;
    }
}

function f2() {
    for (let [{a: x, b: {c: y}}] = [{a: 1, b: {c: 2}}]; x < y; ++x, --y) {
        let s: string = x;
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
        let a = () => x++ + y++;
        if (x == 1)
            break;
        else if (y == 2)
            y = 5;
        else
            return;
    }
}





