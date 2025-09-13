// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/numericClassMembers1.ts`, Apache-2.0 License

class C234 {
  0 = 1; 
  0.0 = 2;
  //~^ ERROR: Duplicate identifier '0'.
}
 
class C235 { 
  0.0 = 1;
 '0' = 2;
  //~^ ERROR: Duplicate identifier '0'.
}

class C236 {
    '0.0' = 1;
    '0' = 2;
}
