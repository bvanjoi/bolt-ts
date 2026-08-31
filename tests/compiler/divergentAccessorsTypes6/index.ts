// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsTypes6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext,dom]
//@compiler-options: declaration

export {};

interface Element {
    get style(): CSSStyleDeclaration;
    set style(cssText: string);
}

declare const element: Element;
element.style = "color: red";
element.style.animationTimingFunction;
element.style = element.style; // error
//~^ ERROR: Type 'CSSStyleDeclaration' is not assignable to type 'string'.

// Now that we don't check for getter/setter assignability, we should
// ensure the setter annotation is actually checked even if it's never observed.

type Fail<T extends never> = T;
interface I1 {
    get x(): number;
    set x(value: Fail<string>);
    //~^ ERROR: Type 'string' does not satisfy the constraint 'never'.
}
const o1 = {
    get x(): number { return 0; },
    set x(value: Fail<string>) {}
    //~^ ERROR: Type 'string' does not satisfy the constraint 'never'.
}

// A setter annotation still implies the getter return type.

const o2 = {
    get p1() { return 0; }, // error - no annotation means type is implied from the setter annotation
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
    set p1(value: string) {},

    get p2(): number { return 0; }, // ok - explicit annotation
    set p2(value: string) {},
};
