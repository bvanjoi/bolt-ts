// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noExcessiveStackDepthError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

interface FindOperator<T> {
    foo: T;
}

type FindConditions<T> = {
    [P in keyof T]?: FindConditions<T[P]> | FindOperator<FindConditions<T[P]>>;
};

function foo<Entity>() {
    var x: FindConditions<any>;
    var x: FindConditions<Entity>;  // Excessive stack depth error not expected here
    //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'FindConditions<any>', but here has type 'FindConditions<Entity>'.
}
