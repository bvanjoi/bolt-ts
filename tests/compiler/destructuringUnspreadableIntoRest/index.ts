// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringUnspreadableIntoRest.ts`, Apache-2.0 License

//@compiler-options: target=es6

class A {
    constructor(
        public publicProp: string,
        private privateProp: string,
        protected protectedProp: string,
    ) {}

    get getter(): number {
        return 1;
    }

    set setter(_v: number) {}

    method() {
        const {                 ...rest1 } = this;
        const {                 ...rest2 } = this as A;
        const { publicProp: _1, ...rest3 } = this;
        const { publicProp: _2, ...rest4 } = this as A;

        rest1.publicProp;
        rest2.publicProp;
        rest3.publicProp;
        //~^ ERROR: Property 'publicProp' does not exist on type 'Omit<A, "publicProp" | "getter" | "setter" | "method">'.
        rest4.publicProp;
        //~^ ERROR: Property 'publicProp' does not exist on type '{ }'.

        rest1.privateProp;
        //~^ ERROR: Property 'privateProp' does not exist on type 'Omit<A, "getter" | "setter" | "method">'.
        rest2.privateProp;
        //~^ ERROR: Property 'privateProp' does not exist on type '{ publicProp: string; }'.
        rest3.privateProp;
        //~^ ERROR: Property 'privateProp' does not exist on type 'Omit<A, "publicProp" | "getter" | "setter" | "method">'.
        rest4.privateProp;
        //~^ ERROR: Property 'privateProp' does not exist on type '{ }'.

        rest1.protectedProp;
        //~^ ERROR: Property 'protectedProp' does not exist on type 'Omit<A, "getter" | "setter" | "method">'.
        rest2.protectedProp;
        //~^ ERROR: Property 'protectedProp' does not exist on type '{ publicProp: string; }'.
        rest3.protectedProp;
        //~^ ERROR: Property 'protectedProp' does not exist on type 'Omit<A, "publicProp" | "getter" | "setter" | "method">'.
        rest4.protectedProp;
        //~^ ERROR: Property 'protectedProp' does not exist on type '{ }'.

        rest1.getter;
        //~^ ERROR: Property 'getter' does not exist on type 'Omit<A, "getter" | "setter" | "method">'.
        rest2.getter;
        //~^ ERROR: Property 'getter' does not exist on type '{ publicProp: string; }'.
        rest3.getter;
        //~^ ERROR: Property 'getter' does not exist on type 'Omit<A, "publicProp" | "getter" | "setter" | "method">'.
        rest4.getter;
        //~^ ERROR: Property 'getter' does not exist on type '{ }'.

        rest1.setter;
        //~^ ERROR: Property 'setter' does not exist on type 'Omit<A, "getter" | "setter" | "method">'.
        rest2.setter;
        //~^ ERROR: Property 'setter' does not exist on type '{ publicProp: string; }'.
        rest3.setter;
        //~^ ERROR: Property 'setter' does not exist on type 'Omit<A, "publicProp" | "getter" | "setter" | "method">'.
        rest4.setter;
        //~^ ERROR: Property 'setter' does not exist on type '{ }'.

        rest1.method;
        //~^ ERROR: Property 'method' does not exist on type 'Omit<A, "getter" | "setter" | "method">'.
        rest2.method;
        //~^ ERROR: Property 'method' does not exist on type '{ publicProp: string; }'.
        rest3.method;
        //~^ ERROR: Property 'method' does not exist on type 'Omit<A, "publicProp" | "getter" | "setter" | "method">'.
        rest4.method;
        //~^ ERROR: Property 'method' does not exist on type '{ }'.
    }
}

function destructure<T extends A>(x: T) {
    const {                 ...rest1 } = x;
    const {                 ...rest2 } = x as A;
    const { publicProp: _1, ...rest3 } = x;
    const { publicProp: _2, ...rest4 } = x as A;

    rest1.publicProp;
    rest2.publicProp;
    rest3.publicProp;
    //~^ ERROR: Property 'publicProp' does not exist on type 'Omit<T, "publicProp" | "getter" | "setter" | "method">'.
    rest4.publicProp;
    //~^ ERROR: Property 'publicProp' does not exist on type '{ }'.

    rest1.privateProp;
    //~^ ERROR: Property 'privateProp' does not exist on type 'Omit<T, "getter" | "setter" | "method">'.
    rest2.privateProp;
    //~^ ERROR: Property 'privateProp' does not exist on type '{ publicProp: string; }'.
    rest3.privateProp;
    //~^ ERROR: Property 'privateProp' does not exist on type 'Omit<T, "publicProp" | "getter" | "setter" | "method">'.
    rest4.privateProp;
    //~^ ERROR: Property 'privateProp' does not exist on type '{ }'.

    rest1.protectedProp;
    //~^ ERROR: Property 'protectedProp' does not exist on type 'Omit<T, "getter" | "setter" | "method">'.
    rest2.protectedProp;
    //~^ ERROR: Property 'protectedProp' does not exist on type '{ publicProp: string; }'.
    rest3.protectedProp;
    //~^ ERROR: Property 'protectedProp' does not exist on type 'Omit<T, "publicProp" | "getter" | "setter" | "method">'.
    rest4.protectedProp;
    //~^ ERROR: Property 'protectedProp' does not exist on type '{ }'.

    rest1.getter;
    //~^ ERROR: Property 'getter' does not exist on type 'Omit<T, "getter" | "setter" | "method">'.
    rest2.getter;
    //~^ ERROR: Property 'getter' does not exist on type '{ publicProp: string; }'.
    rest3.getter;
    //~^ ERROR: Property 'getter' does not exist on type 'Omit<T, "publicProp" | "getter" | "setter" | "method">'.
    rest4.getter;
    //~^ ERROR: Property 'getter' does not exist on type '{ }'.

    rest1.setter;
    //~^ ERROR: Property 'setter' does not exist on type 'Omit<T, "getter" | "setter" | "method">'.
    rest2.setter;
    //~^ ERROR: Property 'setter' does not exist on type '{ publicProp: string; }'.
    rest3.setter;
    //~^ ERROR: Property 'setter' does not exist on type 'Omit<T, "publicProp" | "getter" | "setter" | "method">'.
    rest4.setter;
    //~^ ERROR: Property 'setter' does not exist on type '{ }'.

    rest1.method;
    //~^ ERROR: Property 'method' does not exist on type 'Omit<T, "getter" | "setter" | "method">'.
    rest2.method;
    //~^ ERROR: Property 'method' does not exist on type '{ publicProp: string; }'.
    rest3.method;
    //~^ ERROR: Property 'method' does not exist on type 'Omit<T, "publicProp" | "getter" | "setter" | "method">'.
    rest4.method;
    //~^ ERROR: Property 'method' does not exist on type '{ }'.
}
