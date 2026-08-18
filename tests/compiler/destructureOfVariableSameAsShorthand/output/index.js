// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructureOfVariableSameAsShorthand.ts`, Apache-2.0 License
//@compiler-options: target=es2015
async function main() {
  get().then((response) => {
    var body = response.data;
  });
  get().then(({data}) => {});
  var response = await get();
  var body = response.data;
  var {data} = await get();
  var {data: shouldBeNever} = await get();
}