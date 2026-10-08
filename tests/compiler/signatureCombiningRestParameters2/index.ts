// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureCombiningRestParameters2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[dom,esnext]
//@compiler-options: noEmit

interface Console {
  log(message?: any, ...optionalParams: any[]): void;
}

let logs: string[] = [];
let originalLog: typeof console.log;
console.log = (...args) => {
  logs.push(...args);
};
