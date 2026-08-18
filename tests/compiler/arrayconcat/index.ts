// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayconcat.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IOptions {
    name?: string;
    flag?: boolean;
    short?: string;
    usage?: string;
    set?: (s: string) => void;
    type?: string;
    experimental?: boolean;
}

class parser {
	public options: IOptions[];
  //~^ ERROR: Property 'options' has no initializer and is not definitely assigned in the constructor.

	public m() {
		this.options = this.options.sort(function(a, b) {
            var aName = a.name.toLowerCase();
            //~^ ERROR: 'a.name' is possibly 'undefined'.
            //~| ERROR: 'a.name' is possibly 'undefined'.
            var bName = b.name.toLowerCase();
            //~^ ERROR: 'b.name' is possibly 'undefined'.
            //~| ERROR: 'b.name' is possibly 'undefined'.

            if (aName > bName) {
                return 1;
            } else if (aName < bName) {
                return -1;
            } else {
                return 0;
            }
        });
	}
}
