// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveTypeRelations.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Attributes<Keys extends keyof any> = {
    [Key in Keys]: string;
}

class Query<A extends Attributes<keyof A>> {
    multiply<B extends Attributes<keyof B>>(x: B): Query<A & B>;
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}

// Repro from #14940

type ClassName<S> = keyof S;
type ClassNameMap<S> = { [K in keyof S]?: boolean }
type ClassNameObjectMap<S> = object & ClassNameMap<S>;
type ClassNameArg<S> = ClassName<S> | ClassNameObjectMap<S>;

export function css<S extends { [K in keyof S]: string }>(styles: S, ...classNames: ClassNameArg<S>[]): string {
  const args = classNames.map(arg => {
    if (arg == null) {
      return null;
    }
    if (typeof arg == "string") {
      return styles[arg];
    }
    if (typeof arg == "object") {
      return Object.keys(arg).reduce<ClassNameObject>((obj: ClassNameObject, key: keyof S) => {
        //~^ ERROR: Cannot find name 'ClassNameObject'.
        //~| ERROR: Cannot find name 'ClassNameObject'.
        //~| ERROR: No overload matches this call.
        const exportedClassName = styles[key];
        obj[exportedClassName] = (arg as ClassNameMap<S>)[key]; 
        return obj;
      }, {});
    }
  });
  return "";
}
