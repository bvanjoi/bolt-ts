// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ifElseWithStatements1.ts`, Apache-2.0 License

if (true)
  f(); //~ ERROR: Cannot find name 'f'.
else
  f(); //~ ERROR: Cannot find name 'f'.

function foo(): boolean {
  if (true)
      return true;
  else
      return false;
}
