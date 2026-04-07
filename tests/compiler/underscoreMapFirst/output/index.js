
class MyView extends View {
  getDataSeries() {
    var data = this.model.get('data');
    var allSeries = _.pluck(data, 'series');
    return _.map(allSeries, _.first)
  }
}