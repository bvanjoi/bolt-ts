// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowNullTypeAndLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const myNull: null = null;
const objWithValMaybeNull: { val: number | null } = { val: 1 };
const addOne = function (num: number) {
    return num + 1;
}

if (objWithValMaybeNull.val !== null)
    addOne(objWithValMaybeNull.val);
if (objWithValMaybeNull.val !== myNull)
    addOne(objWithValMaybeNull.val);

if (objWithValMaybeNull.val === null)
    addOne(objWithValMaybeNull.val);    // Error
  //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'number'.
if (objWithValMaybeNull.val === myNull)
    addOne(objWithValMaybeNull.val);    // Error
  //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'number'.

function f(x: number | null) {
    if(x === myNull) {
        const s: string = x;  // Error
    //~^ ERROR: Type 'null' is not assignable to type 'string'.
    }
}
