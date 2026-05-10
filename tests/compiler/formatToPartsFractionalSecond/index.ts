// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/formatToPartsFractionalSecond.ts`, Apache-2.0 License

//@compiler-options: target=esnext

new Intl.DateTimeFormat().formatToParts().find((val) => val.type === 'fractionalSecond')
