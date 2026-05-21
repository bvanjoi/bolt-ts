// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ClassTest.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Bar {
    public goo: number;
    public prop1(x) {
        return x;
    }

    constructor (n) { }
}

// new-style class
class Foo  extends Bar {
	foo:number;
	gar = 0;
	zoo:string = "zoo";
	x: any;

	bar() { return 0; }

	private boo();
	private boo(x?) { return x; }

    static statVal = 0;

	constructor();
	constructor(x?, private y?:string, public z?=0) {
    //~^ ERROR: Parameter cannot have question mark and initializer.
        super(x);
		this.x = x;
        this.gar = 5;
	 }
}

var f = new Foo();

declare namespace AmbientMod {
	export class Provide {
		foo:number;
		zoo:string;

		constructor();
		
		private boo();
		bar();
	}
}


//class GetSetMonster {


//  // attack(target) {
//  //     WScript.Echo("Attacks " + target);
//  // }
//  // The contextual keyword "get" followed by an identifier and
//  // a curly body defines a getter in the same way that "get"
//  // defines one in an object literal.
//  // get isAlive() {
//  //   return this.health > 0;
//  // }
 
//  // Likewise, "set" can be used to define setters.
//  set health(value:number) {
//    if (value < 0) {
//      throw new Error('Health must be non-negative.')
//    }
//    this.health = value
//  }
//  get health() { return 0; }

//  constructor(this.name: string, health: number) {
//    this.health = 0;
//  }
//}


//class bar {

//   static fnOverload( );

//   static fnOverload(foo: string){ } // no error

//   constructor(){};    

//}
