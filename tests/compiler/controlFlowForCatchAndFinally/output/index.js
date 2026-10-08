// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowForCatchAndFinally.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[es6]
async function test() {
  var browser = undefined;
  var page = undefined;
  try {
    browser = await test1();
    page = await test2(browser);
    return await page.content();
    ;
  }finally {
    if (page) {
      await page.close();
    // ok
    }
    
    if (browser) {
      await browser.close();
    // ok
    }
    
  }
}
;
class Foo {
  abortController = undefined;
  operation() {
    if (this.abortController !== undefined) {
      this.abortController.abort();
      this.abortController = undefined;
    }
    
    try {
      this.abortController = new Aborter();
    } catch (error) {
      if (this.abortController !== undefined) {
        this.abortController.abort();
      }
      
    // ok
    }
  }
}