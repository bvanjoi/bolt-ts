// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionOfArraysFilterCall.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict

interface Fizz {
    id: number;
    fizz: string;
}

interface Buzz {
    id: number;
    buzz: string;
}

([] as Fizz[] | Buzz[]).filter(item => item.id < 5);
([] as Fizz[] | readonly Buzz[]).filter(item => item.id < 5); 

([] as Fizz[] | Buzz[]).find(item => item);
declare function isFizz(x: unknown): x is Fizz;
([] as Fizz[] | Buzz[]).find(isFizz);
declare function isBuzz(x: unknown): x is Buzz;
([] as Fizz[] | Buzz[]).find(isBuzz);

([] as Fizz[] | Buzz[]).every(item => item.id < 5);

([] as Fizz[] | Buzz[]).reduce(item => item);


([] as [Fizz] | readonly [Buzz?]).filter(item => item?.id < 5);
//~^ ERROR: 'item.id' is possibly 'undefined'.
//~| ERROR: 'item.id' is possibly 'undefined'.
//~| ERROR: 'item.id' is possibly 'undefined'.
