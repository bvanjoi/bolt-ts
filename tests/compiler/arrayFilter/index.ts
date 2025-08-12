// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/arrayFilter.ts`, Apache-2.0 License

var foo = [
    { name: 'bar' },
    { name: null },
    { name: 'baz' }
]

foo.filter(x => x.name); //should accepted all possible types not only boolean! 
