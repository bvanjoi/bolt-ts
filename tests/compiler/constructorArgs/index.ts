// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorArgs.ts`, Apache-2.0 License

interface Options {
  value: number;
 }
 
 class Super {
  constructor(value:number) {
  }
 }
 
 class Sub extends Super {
  constructor(public options:Options) {
   super(options.value);
  } 
 }
 