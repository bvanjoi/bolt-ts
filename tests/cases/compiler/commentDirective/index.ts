// @ts-expect-error
let a: string = 42;

// @ts-expect-error   //~ ERROR: Unused '@ts-expect-error' directive.
let b: string = '42';

// @ts-ignore
let c: string = 42;

// @ts-ignore
let d: string = '42';

// @ts-expect-error
// 
let e: string = 42;

// @ts-expect-error
let f = notExist;

// @ts-expect-erroeeeeeee
let g: string = 42;    //~ ERROR: Type 'number' is not assignable to type 'string'.

// @ts-expect-errorrrrrrr
let g1: string = 42;

// @ts-ignoreeeeee
let h: string = 42;

// @ts-ignoraaaaaaa
let h1: string = 42;   //~ ERROR: Type 'number' is not assignable to type 'string'.

// @ts-expect-error

let i: string = 42

// @ts-expect-error
// something
let j: string = 42;

// @ts-expect-error
       // something
let k: string = 42;

//@ts-expect-error
let l: string = 42

//                 @ts-expect-error
let m: string = 42

// @ts-expect-error // @ts-expect-error  @ts-expect-error
let n: string = 42