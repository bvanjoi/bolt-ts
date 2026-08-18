// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mixinPrivateAndProtected.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Constructor<T> = new(...args: any[]) => T;

class A {
    public pb: number = 2;
    protected ptd: number = 1;
    private pvt: number = 0;
}

function mixB<T extends Constructor<{}>>(Cls: T) {
    return class extends Cls {
        protected ptd: number = 10;
        private pvt: number = 0;
    };
}

function mixB2<T extends Constructor<A>>(Cls: T) {
    return class extends Cls {
        protected ptd: number = 10;
    };
}

const
    AB = mixB(A),
    AB2 = mixB2(A);

function mixC<T extends Constructor<{}>>(Cls: T) {
    return class extends Cls {
        protected ptd: number = 100;
        private pvt: number = 0;
    };
}

const
    AB2C = mixC(AB2),
    ABC = mixC(AB);

const
    a = new A(),
    ab = new AB(),
    abc = new ABC(),
    ab2c = new AB2C();

a.pb.toFixed();
a.ptd.toFixed();    // Error
//~^ ERROR: Property 'ptd' is protected and only accessible within class 'A' and its subclasses.
a.pvt.toFixed();    // Error
//~^ ERROR: Property 'pvt' is private and only accessible within class 'A'.

ab.pb.toFixed();
//~^ ERROR: The intersection '(Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.
ab.ptd.toFixed();   // Error
//~^ ERROR: The intersection '(Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.
ab.pvt.toFixed();   // Error
//~^ ERROR: The intersection '(Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.

abc.pb.toFixed();
//~^ ERROR: The intersection '(Anonymous class)<(typeof (Anonymous class)) & (typeof A)> & (Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.
abc.ptd.toFixed();  // Error
//~^ ERROR: The intersection '(Anonymous class)<(typeof (Anonymous class)) & (typeof A)> & (Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.
abc.pvt.toFixed();  // Error
//~^ ERROR: The intersection '(Anonymous class)<(typeof (Anonymous class)) & (typeof A)> & (Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.

ab2c.pb.toFixed();
//~^ ERROR: The intersection '(Anonymous class)<(typeof (Anonymous class)) & (typeof A)> & (Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.
ab2c.ptd.toFixed(); // Error
//~^ ERROR: The intersection '(Anonymous class)<(typeof (Anonymous class)) & (typeof A)> & (Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.
ab2c.pvt.toFixed(); // Error
//~^ ERROR: The intersection '(Anonymous class)<(typeof (Anonymous class)) & (typeof A)> & (Anonymous class)<typeof A> & A' was reduced to 'never' because property 'pvt' has conflicting types in some constituents.

// Repro from #13924

class Person {
	constructor(public name: string) {}

	protected myProtectedFunction() {
		// do something
	}
}

function PersonMixin<T extends Constructor<Person>>(Base: T) {
	return class extends Base {
		constructor(...args: any[]) {
			super(...args);
		}

		myProtectedFunction() {
			super.myProtectedFunction();
			// do more things
		}
	};
}

class Customer extends PersonMixin(Person) {
	accountBalance: number;
  //~^ ERROR: Property 'accountBalance' has no initializer and is not definitely assigned in the constructor.
    f() {
    }
}
