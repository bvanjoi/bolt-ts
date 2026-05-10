// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/incorrectNumberOfTypeArgumentsDuringErrorReporting.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ObjA {
  y?:string,
}

interface ObjB {[key:string]:any}

interface Opts<A, B> {a:A, b:B}

const fn = <
  A extends ObjA,
  B extends ObjB = ObjB
>(opts:Opts<A, B>):string => 'Z'

interface MyObjA {
  x:string,
}

fn<MyObjA>({  //~ERROR: Type 'MyObjA' has no properties in common with type 'ObjA'.
  a: {x: 'X', y: 'Y'},
  b: {},
})
