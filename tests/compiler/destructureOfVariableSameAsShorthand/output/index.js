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