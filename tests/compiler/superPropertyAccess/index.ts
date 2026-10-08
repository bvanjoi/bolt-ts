// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccess.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class MyBase {
    m1(a: string) { return a; }
    private p1() { }
    m2: () => void = function () { }
    d1: number = 42;
    private d2: number = 42;
    get value() {return 0 }
    set value(v: number) { }
}


class MyDerived extends MyBase {

    foo() {
        super.m1("hi");                                     // Should be allowed, method on base prototype

        var l2 = super.m1.bind(this);                       // Should be allowed, can access properties as well as invoke

        var x: (a: string) => string = super.m1;            // Should be allowed, can assign to var with compatible signature

        super.m2.bind(this);                                // Should error, instance property, not a public instance member function
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES5]| ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES5]| ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES2015]^^^^ ERROR: Class field 'm2' defined by the parent class is not accessible in the child class via super.
        //~[target=ES2015]| ERROR: Class field 'm2' defined by the parent class is not accessible in the child class via super.
        //~[target=ES2015]| ERROR: Class field 'm2' defined by the parent class is not accessible in the child class via super.
        
        super.p1();                                         // Should error, private not public instance member function
        //~^ ERROR: Property 'p1' is private and only accessible within class 'MyBase'.

        var l1 = super.d1;                                  // Should error, instance data property not a public instance member function
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES2015]^^ ERROR: Class field 'd1' defined by the parent class is not accessible in the child class via super.

        var l1 = super.d2;                                  // Should error, instance data property not a public instance member function
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES2015]^^ ERROR: Class field 'd2' defined by the parent class is not accessible in the child class via super.

        super.m1 = function (a: string) { return ""; };     // Should be allowed, we will not restrict assignment

        super.value = 0;                                    // Should error, instance data property not a public instance member function
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        
        var z = super.value;                                // Should error, instance data property not a public instance member function
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
    }
}