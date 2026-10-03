var classesByRow = {};
for ( var row of ['1', '2', '3', '4', '5']) {
  class RowClass {
    row = row;
    static factory = () => (new RowClass());
  }
  classesByRow[row] = RowClass;
}