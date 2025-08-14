// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/instantiateContextuallyTypedGenericThis.ts`, Apache-2.0 License

//@ run-fail


interface JQuery {
  each<T>(
      collection: T[], callback: (this: T, dit: T) => T
  ): T[];
}

let $: JQuery;
let lines: string[];
$.each(lines, function(dit) {
  return dit.charAt(0) + this.charAt(1);
});
