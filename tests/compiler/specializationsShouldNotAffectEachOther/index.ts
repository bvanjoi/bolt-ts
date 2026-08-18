// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializationsShouldNotAffectEachOther.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface Series  {
    data: string[];
}

var series: Series;


function foo() {

    var seriesExtent = (series) => null;

    var series2: number[];

    series2.map(seriesExtent);
    return null;
}


var keyExtent2: any[] = series.data.map(function (d: string) { return d; });