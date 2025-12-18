// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/blockScopedBindingsReassignedInLoop5.ts`, Apache-2.0 License


for (let x = 1, y = 2; x < y; ++x, --y) {
    let a = () => x++ + y++;
    if (x == 1) 
        break;
    else 
        y = 5;
}