// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instantiatedReturnTypeContravariance.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface B<T> {

// name: string;

x(): T;

}
 
class c {

foo(): B<void> {

return null;
//~^ ERROR: 'null' is not assignable to type 'B<void>'.

}

}
 
class d extends c {

foo(): B<number> {

return null;
//~^ ERROR: 'null' is not assignable to type 'B<number>'.

}

}

 
