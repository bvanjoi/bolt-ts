// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/baseTypeOrderChecking.ts`, Apache-2.0 License

var someVariable: Class4<Class2>;

 

class Class1

{

}

 

class Class2 extends Class1

{

}

 

class Class3<T>

{

               public memberVariable: Class2;

}

 

class Class4<T> extends Class3<T>

{

}
