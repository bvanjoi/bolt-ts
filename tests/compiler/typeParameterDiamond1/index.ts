// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterDiamond1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function diamondTop<Top>() {
    function diamondMiddle<T extends Top, U extends Top>() {
        function diamondBottom<Bottom extends T | U>() {
            var top: Top;
            var middle: T | U;
            var bottom: Bottom;

            top = middle;
            //~^ ERROR: Variable 'middle' is used before being assigned.
            middle = bottom;
            //~^ ERROR: Variable 'bottom' is used before being assigned.
            top = bottom;
            //~^ ERROR: Variable 'bottom' is used before being assigned.
        }
    }
}