// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/underscoreMapFirst.ts`, Apache-2.0 License
//@compiler-options: target=es2015

class MyView extends View {
  getDataSeries() {
    var data = this.model.get('data');
    var allSeries = _.pluck(data, 'series');
    return _.map(allSeries, _.first);
  }
}