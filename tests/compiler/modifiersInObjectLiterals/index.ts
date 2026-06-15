// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modifiersInObjectLiterals.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let data = {
	public foo: 'hey',	//~ERROR: Modifier cannot be used here.
	private bar: 'nay',	//~ERROR: Modifier cannot be used here.
	protected baz: 'oh my',	//~ERROR: Modifier cannot be used here.
	abstract noWay: 'yes'	//~ERROR: Modifier cannot be used here.	
};

data.foo + data.bar + data.baz + data.noWay
