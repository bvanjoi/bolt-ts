// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/intersectionSatisfiesConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[esnext]

interface FirstInterface {
    commonProperty: number
}

interface SecondInterface {
    commonProperty: number
}

const myFirstFunction = <T extends FirstInterface | SecondInterface>(param1: T) => {
    const newParam: T & { otherProperty: number } = Object.assign(param1, { otherProperty: 3 })
    mySecondFunction(newParam)
}

const mySecondFunction = <T extends { commonProperty: number, otherProperty: number }>(newParam: T) => {
    return newParam
}