// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowTypeByInstanceof.ts`, Apache-2.0 License

//@compiler-options: target=es2015

    class Match {
        public range(): any {
            return undefined;
        }
    }

    class FileMatch {
        public resource(): any {
            return undefined;
        }
    }

type FileMatchOrMatch = FileMatch | Match;


let elementA: FileMatchOrMatch, elementB: FileMatchOrMatch;

if (elementA instanceof FileMatch && elementB instanceof FileMatch) {
  //~^ ERROR: Variable 'elementA' is used before being assigned.
  //~| ERROR: Variable 'elementB' is used before being assigned.
  //~| ERROR: Variable 'elementB' is used before being assigned.
    let a = elementA.resource().path;
    let b = elementB.resource().path;
} else if (elementA instanceof Match && elementB instanceof Match) {
  //~^ ERROR: Variable 'elementA' is used before being assigned.
  //~| ERROR: Variable 'elementB' is used before being assigned.
  //~| ERROR: Variable 'elementB' is used before being assigned.
    let a = elementA.range();
    let b = elementB.range();
}
