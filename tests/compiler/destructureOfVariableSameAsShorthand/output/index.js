// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructureOfVariableSameAsShorthand.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// https://github.com/microsoft/TypeScript/issues/38969
async function main() {
  // These work examples as expected
  get().then((response) => {
    // body is never
    var body = response.data;
  });
  get().then(({data}) => // data is never
  {});
  var response = await get// body is never
  ();
  var body = response// data is never
  .data;
  var {data} = await get();
  // The following did not work as expected.
  // shouldBeNever should be never, but was any
  var {data: shouldBeNever} = await get();
}