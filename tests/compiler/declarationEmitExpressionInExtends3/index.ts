// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitExpressionInExtends3.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration

export class ExportedClass<T> {
	x: T;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

class LocalClass<T, U> {
    x: T;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    y: U;
  //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
}

export interface ExportedInterface {
    x: number;
}

interface LocalInterface {
    x: number;
}

function getLocalClass<T>(c: T) {
    return LocalClass;
}

function getExportedClass<T>(c: T) {
    return ExportedClass;
}



export class MyClass extends getLocalClass<LocalInterface>(undefined)<string, number> { // error LocalClass is inaccisible
  //~^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'LocalInterface'.
}


export class MyClass2 extends getExportedClass<LocalInterface>(undefined)<string> { // OK
  //~^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'LocalInterface'.
}


export class MyClass3 extends getExportedClass<LocalInterface>(undefined)<LocalInterface> { // Error LocalInterface is inaccisble
  //~^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'LocalInterface'.
}


export class MyClass4 extends getExportedClass<LocalInterface>(undefined)<ExportedInterface> { // OK
  //~^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'LocalInterface'.
}
