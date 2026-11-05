export default { a: true }
export type X = typeof default; // expect error
//~^ ERROR: Cannot find name 'default'.
