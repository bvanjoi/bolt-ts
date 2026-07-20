// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypesInIndexedAccessTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

type UserArgs = {
  select?: boolean
};

type Subset<T, U> = { [key in keyof T]: key extends keyof U ? T[key] : never };

declare function withBoundary<T extends UserArgs>(args?: Subset<T, UserArgs>): T;
declare function withoutBoundary<T extends UserArgs>(args?: T): T;

const boundaryResult = withBoundary({
  select: true,
});

const withoutBoundaryResult = withoutBoundary({
  select: true,
});
