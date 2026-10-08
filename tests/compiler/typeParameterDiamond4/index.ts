// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterDiamond4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function diamondTop<Top>() {
    function diamondMiddle<T, U>() {
        function diamondBottom<Bottom extends Top | T | U>() {
            var top!: Top;
            var middle!: Top | T | U;
            var bottom!: Bottom;

            top = middle;
            //~^ ERROR: Type 'Top | T | U' is not assignable to type 'Top'.
            middle = bottom;
            top = bottom;
            //~^ ERROR: Type 'Bottom' is not assignable to type 'Top'.
        }
    }
}

type G<B> = B extends 'value' ? 42 : '42'

declare function fff<const B>(b: B): G<B>;

function test_test() {
    let a: '42' = fff('value');
    //~^ ERROR: Type 'number' is not assignable to type '"42"'.
    let b: 42 = fff('value');
}