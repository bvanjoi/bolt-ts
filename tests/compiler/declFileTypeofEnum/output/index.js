var days = {};
(function (days) {

  days[days['monday'] = 0] = 'monday'
  days[days['tuesday'] = 0] = 'tuesday'
  days[days['wednesday'] = 0] = 'wednesday'
  days[days['thursday'] = 0] = 'thursday'
  days[days['friday'] = 0] = 'friday'
  days[days['saturday'] = 0] = 'saturday'
  days[days['sunday'] = 0] = 'sunday'
})(days);
var weekendDay = days.saturday;
var daysOfMonth = days;
var daysOfYear;