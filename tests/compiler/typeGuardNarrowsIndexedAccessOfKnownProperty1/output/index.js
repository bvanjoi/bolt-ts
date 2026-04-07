function area(s) {
  switch (s['dash-ok']) {
    case 'square':
      return s['square-size'] * s['square-size']
    
    case 'rectangle':
      return s.width * s['height']
    
    case 'circle':
      return Math.PI * s['radius'] * s.radius
    
  }
}
function subarea(s) {
  switch (s[0]['sub'].under['shape']['dash-ok']) {
    case 'square':
      return s[0].sub.under.shape['square-size'] * s[0].sub.under.shape['square-size']
    
    case 'rectangle':
      return s[0]['sub']['under']['shape']['width'] * s[0]['sub']['under']['shape'].height
    
    case 'circle':
      return Math.PI * s[0].sub.under['shape'].radius * s[0]['sub'].under.shape['radius']
    
  }
}
function check(z, c) {
  z[0];
  switch (z[0]) {
    case 'xx':
      var xx = z[1];
      break;
    
    case 'yy':
      var yy = z[1];
      break;
    
  }
  c[0];
  switch (c[0]) {
    case 'aa':
      var aa = c[1];
      break;
    
    case 'bb':
      var bb = c[1];
      break;
    
  }
}
export function g(pair) {
  return pair[1] ? pair[1] : 'nope'
}