// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeMultiInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface Style {
    flashy: any;
}

declare function mergeStyleSets<K extends string>(
    ...cssSets: { [P in K]?: Style }[]): { [P in K]: Style };

// Expected:
//   let x: {
//       a: Style;
//       b: Style;
//   }
let x = mergeStyleSets(
    {},
    {
        a: { flashy: true },
    },
    {
        b: { flashy: true },
    },
)