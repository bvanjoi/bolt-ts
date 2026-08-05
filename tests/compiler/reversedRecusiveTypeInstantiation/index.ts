// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reversedRecusiveTypeInstantiation.ts`, Apache-2.0 License

interface A<StringArgPos1, NumberArgPos2> {
   xPos1 : StringArgPos1
   yPos2 : NumberArgPos2
   zPos2Pos1 : A<NumberArgPos2, StringArgPos1>
}

var a : A<string, number>
a.zPos2Pos1.xPos1 = 1
//~^ ERROR: Variable 'a' is used before being assigned.

