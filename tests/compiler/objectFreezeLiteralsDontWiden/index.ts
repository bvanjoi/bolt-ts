// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectFreezeLiteralsDontWiden.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const PUPPETEER_REVISIONS = Object.freeze({
  chromium: '1011831',
  firefox: 'latest',
});

let preferredRevision = PUPPETEER_REVISIONS.chromium;
preferredRevision = PUPPETEER_REVISIONS.firefox;
//~^ ERROR:  Type '"latest"' is not assignable to type '"1011831"'.
