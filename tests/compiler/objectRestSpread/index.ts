// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectRestSpread.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2017
//@compiler-options: lib=[es2018]

let obj = {};

({...obj});
let {
    prop = { ...obj },
    more = { ...obj } = { ...obj },
    ['' + 'other']: other = { ...obj },
    yetAnother: {nested: { ['nested' + 'prop']: nestedProp = { ...obj }, ...nestedRest } = { ...obj }} = { ...obj },
    fn = async function*() {},
    ...props
} = {} as any;

({
    prop = { ...obj },
    ['' + 'other']: other = { ...obj },
    ...props
} = {} as any)

function test({
    prop = { ...obj },
    ...props
}) {}