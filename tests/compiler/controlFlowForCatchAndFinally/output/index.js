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
    }
    
    if (browser) {
      await browser.close();
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
      
    }
  }
}