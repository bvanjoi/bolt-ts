// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateStructuralMatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015

getResults1([]);
getResults1({data: []});

getResults2([]);
getResults2({data: []});

type Result = { value: string };
type Results = Result[];

function isResponseInData<T>(value: T | { data: T}): value is { data: T } {
    return value.hasOwnProperty('data');
    //~^ ERROR: Property 'hasOwnProperty' does not exist on type 'T | { data: T; }'.
}

function getResults1(value: Results | { data: Results }): Results {
    return isResponseInData(value) ? value.data : value;
}

function isPlainResponse<T>(value: T | { data: T}): value is T {
    return !value.hasOwnProperty('data');
    //~^ ERROR: Property 'hasOwnProperty' does not exist on type 'T | { data: T; }'.
}

function getResults2(value: Results | { data: Results }): Results {
    return isPlainResponse(value) ? value : value.data;
}