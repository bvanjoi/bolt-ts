// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexTypeCheck.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Red {
	[n:number]; // ok
  //~^ ERROR: An index signature must have a type annotation.
	[s:string]; // ok
  //~^ ERROR: An index signature must have a type annotation.
}

interface Blue {
	[n:number]: any; // ok
	[s:string]: any; // ok
}

interface Yellow {
	[n:number]: Red; // ok
	[s:string]: Red; // ok
}

interface Orange {
	[n:number]: number; // ok
  //~^ ERROR: 'number' index type 'number' is not assignable to 'string' index type 'string'.
	[s:string]: string; // error
}

interface Green {
	[n:number]: Orange; // error
  //~^ ERROR: 'number' index type 'Orange' is not assignable to 'string' index type 'Yellow'.
	[s:string]: Yellow; // ok
}

interface Cyan {
	[n:number]: number; // error
  //~^ ERROR: 'number' index type 'number' is not assignable to 'string' index type 'string'.
	[s:string]: string; // ok
}

interface Purple {
	[n:number, s:string]; // error
  //~^ ERROR: An index signature cannot have a trailing comma.
  //~| ERROR: Expected ']'.
  //~| ERROR: An index signature must have a type annotation.
  //~| ERROR: Property or signature expected.
  //~| ERROR: Property or signature expected.
}

interface Magenta {
	[p:Purple]; // error
  //~^ ERROR: An index signature must have a type annotation.
  //~| ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
}

declare var yellow: Yellow;
declare var blue: Blue;
var s = "some string";

yellow[5]; // ok
yellow["hue"]; // ok
yellow[<any>{}]; // ok

s[0]; // error
s["s"]; // ok
s[<any>{}]; // ok

yellow[blue]; // error
//~^ ERROR: Type 'Blue' cannot be used as an index type.
declare var x:number[];
x[0];

class Benchmark {

    public results: { [x:string]: any; } = <{ [x:string]: any; }>{};

    public addTimingFor(name: string, timing: number) {
        this.results[name] = this.results[name];
    }
}