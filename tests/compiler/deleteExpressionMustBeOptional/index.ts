// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deleteExpressionMustBeOptional.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@[strict=true]   compiler-options: strict
//@[strict=false]  compiler-options: strict=false

interface Foo {
    a: number
    b: number | undefined
    c: number | null
    d?: number
    e: number | undefined | null
    f?: number | undefined | null
    g: unknown
    h: any
    i: never
}

interface AA {
    [s: string]: number
}

type BB = {
    [P in keyof any]: number
}

declare const f: Foo
declare const a: AA
declare const b: BB

delete f.a
//~[strict=true]^ ERROR: The operand of a 'delete' operator must be optional.
delete f.b
delete f.c
//~[strict=true]^ ERROR: The operand of a 'delete' operator must be optional.
delete f.d
delete f.e
delete f.f
delete f.g
delete f.h
delete f.i
delete f.j
//~^ ERROR: Property 'j' does not exist on type 'Foo'.

delete a.a
delete a.b

delete b.a
delete b.b
