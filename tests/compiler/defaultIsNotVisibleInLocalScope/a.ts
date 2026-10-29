export default function () {
    return true;
}
export type X = typeof default;  // expect error
//~^ ERROR: Cannot find name 'default'.