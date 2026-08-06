// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorForUsingPropertyOfTypeAsType03.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Test1 {
    enum Color {
        Red,
        Green,
        Blue
    }

    type C1 = Color;
    type C2 = typeof Color;

    let a1: Color.Red.toString;
    //~^ ERROR: 'Color.Red' refers to a value, but is being used as a type here. Did you mean 'typeof Color.Red'?
    let a2: Color.Red["toString"];
    let a3: Color["Red"]["toString"];
    //~^ ERROR: Property '"Red"' does not exist on type 'Color'.

    //let b1: (typeof Color).Red.toString;
    //let b2: (typeof Color).Red["toString"];
    let b3: (typeof Color)["Red"]["toString"];

    let c1: C1.Red.toString;
    //~^ ERROR: Cannot find name 'C1'.
    let c2: C1.Red["toString"];
    //~^ ERROR: Cannot find name 'C1'.
    let c3: C1["Red"]["toString"];
    //~^ ERROR: Property '"Red"' does not exist on type 'Color'.

    let d1: C2.Red.toString;
    //~^ ERROR: Cannot find name 'C2'.
    let d2: C2.Red["toString"];
    //~^ ERROR: Cannot find name 'C2'.
    let d3: C2["Red"]["toString"];
}