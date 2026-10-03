

async function A() {
  var a = await p;
  return a;
}
async function B() {
  var a = await p;
  return 1;
}
async function C() {
  try {
    var a = await p;
    return 1;
  } catch (e) {
    return 'error';
  }
}
async function D() {
  try {
    var a = await p;
    return 1;
  } catch (e) {}
}
async function E() {
  try {
    var a = await p;
    return 1;
  } catch (e) {
    throw Error()
  }
}
async function F() {
  try {
    var a = await p;
    return 1;
  } catch (e) {
    return Promise.reject(Error());
  }
}
async function G() {
  try {
    var a = await p;
    return a;
  } catch (e) {
    return ;
  }
}
async function H() {
  try {
    var a = await p;
    return a;
  } catch (e) {
    throw Error()
  }
}
async function I() {
  try {
    var a = await p;
    return a;
  } catch (e) {
    return Promise.reject(Error());
  }
}
var p00 = p.catch();
var p01 = p.then();
var p10 = p.catch(undefined);
var p11 = p.catch(null);
var p12 = p.catch(() => (1));
var p13 = p.catch(() => (x));
var p14 = p.catch(() => (undefined));
var p15 = p.catch(() => (null));
var p16 = p.catch(() => {});
var p17 = p.catch(() => {
  throw 1
});
var p18 = p.catch(() => (Promise.reject(1)));
var p19 = p.catch(() => (Promise.resolve(1)));
var p20 = p.then(undefined);
var p21 = p.then(null);
var p22 = p.then(() => (1));
var p23 = p.then(() => (x));
var p24 = p.then(() => (undefined));
var p25 = p.then(() => (null));
var p26 = p.then(() => {});
var p27 = p.then(() => {
  throw 1
});
var p28 = p.then(() => (Promise.resolve(1)));
var p29 = p.then(() => (Promise.reject(1)));
var p30 = p.then(undefined, undefined);
var p31 = p.then(undefined, null);
var p32 = p.then(undefined, () => (1));
var p33 = p.then(undefined, () => (x));
var p34 = p.then(undefined, () => (undefined));
var p35 = p.then(undefined, () => (null));
var p36 = p.then(undefined, () => {});
var p37 = p.then(undefined, () => {
  throw 1
});
var p38 = p.then(undefined, () => (Promise.resolve(1)));
var p39 = p.then(undefined, () => (Promise.reject(1)));
var p40 = p.then(null, undefined);
var p41 = p.then(null, null);
var p42 = p.then(null, () => (1));
var p43 = p.then(null, () => (x));
var p44 = p.then(null, () => (undefined));
var p45 = p.then(null, () => (null));
var p46 = p.then(null, () => {});
var p47 = p.then(null, () => {
  throw 1
});
var p48 = p.then(null, () => (Promise.resolve(1)));
var p49 = p.then(null, () => (Promise.reject(1)));
var p50 = p.then(() => ('1'), undefined);
var p51 = p.then(() => ('1'), null);
var p52 = p.then(() => ('1'), () => (1));
var p53 = p.then(() => ('1'), () => (x));
var p54 = p.then(() => ('1'), () => (undefined));
var p55 = p.then(() => ('1'), () => (null));
var p56 = p.then(() => ('1'), () => {});
var p57 = p.then(() => ('1'), () => {
  throw 1
});
var p58 = p.then(() => ('1'), () => (Promise.resolve(1)));
var p59 = p.then(() => ('1'), () => (Promise.reject(1)));
var p60 = p.then(() => (x), undefined);
var p61 = p.then(() => (x), null);
var p62 = p.then(() => (x), () => (1));
var p63 = p.then(() => (x), () => (x));
var p64 = p.then(() => (x), () => (undefined));
var p65 = p.then(() => (x), () => (null));
var p66 = p.then(() => (x), () => {});
var p67 = p.then(() => (x), () => {
  throw 1
});
var p68 = p.then(() => (x), () => (Promise.resolve(1)));
var p69 = p.then(() => (x), () => (Promise.reject(1)));
var p70 = p.then(() => (undefined), undefined);
var p71 = p.then(() => (undefined), null);
var p72 = p.then(() => (undefined), () => (1));
var p73 = p.then(() => (undefined), () => (x));
var p74 = p.then(() => (undefined), () => (undefined));
var p75 = p.then(() => (undefined), () => (null));
var p76 = p.then(() => (undefined), () => {});
var p77 = p.then(() => (undefined), () => {
  throw 1
});
var p78 = p.then(() => (undefined), () => (Promise.resolve(1)));
var p79 = p.then(() => (undefined), () => (Promise.reject(1)));
var p80 = p.then(() => (null), undefined);
var p81 = p.then(() => (null), null);
var p82 = p.then(() => (null), () => (1));
var p83 = p.then(() => (null), () => (x));
var p84 = p.then(() => (null), () => (undefined));
var p85 = p.then(() => (null), () => (null));
var p86 = p.then(() => (null), () => {});
var p87 = p.then(() => (null), () => {
  throw 1
});
var p88 = p.then(() => (null), () => (Promise.resolve(1)));
var p89 = p.then(() => (null), () => (Promise.reject(1)));
var p90 = p.then(() => {}, undefined);
var p91 = p.then(() => {}, null);
var p92 = p.then(() => {}, () => (1));
var p93 = p.then(() => {}, () => (x));
var p94 = p.then(() => {}, () => (undefined));
var p95 = p.then(() => {}, () => (null));
var p96 = p.then(() => {}, () => {});
var p97 = p.then(() => {}, () => {
  throw 1
});
var p98 = p.then(() => {}, () => (Promise.resolve(1)));
var p99 = p.then(() => {}, () => (Promise.reject(1)));
var pa0 = p.then(() => {
  throw 1
}, undefined);
var pa1 = p.then(() => {
  throw 1
}, null);
var pa2 = p.then(() => {
  throw 1
}, () => (1));
var pa3 = p.then(() => {
  throw 1
}, () => (x));
var pa4 = p.then(() => {
  throw 1
}, () => (undefined));
var pa5 = p.then(() => {
  throw 1
}, () => (null));
var pa6 = p.then(() => {
  throw 1
}, () => {});
var pa7 = p.then(() => {
  throw 1
}, () => {
  throw 1
});
var pa8 = p.then(() => {
  throw 1
}, () => (Promise.resolve(1)));
var pa9 = p.then(() => {
  throw 1
}, () => (Promise.reject(1)));
var pb0 = p.then(() => (Promise.resolve('1')), undefined);
var pb1 = p.then(() => (Promise.resolve('1')), null);
var pb2 = p.then(() => (Promise.resolve('1')), () => (1));
var pb3 = p.then(() => (Promise.resolve('1')), () => (x));
var pb4 = p.then(() => (Promise.resolve('1')), () => (undefined));
var pb5 = p.then(() => (Promise.resolve('1')), () => (null));
var pb6 = p.then(() => (Promise.resolve('1')), () => {});
var pb7 = p.then(() => (Promise.resolve('1')), () => {
  throw 1
});
var pb8 = p.then(() => (Promise.resolve('1')), () => (Promise.resolve(1)));
var pb9 = p.then(() => (Promise.resolve('1')), () => (Promise.reject(1)));
var pc0 = p.then(() => (Promise.reject('1')), undefined);
var pc1 = p.then(() => (Promise.reject('1')), null);
var pc2 = p.then(() => (Promise.reject('1')), () => (1));
var pc3 = p.then(() => (Promise.reject('1')), () => (x));
var pc4 = p.then(() => (Promise.reject('1')), () => (undefined));
var pc5 = p.then(() => (Promise.reject('1')), () => (null));
var pc6 = p.then(() => (Promise.reject('1')), () => {});
var pc7 = p.then(() => (Promise.reject('1')), () => {
  throw 1
});
var pc8 = p.then(() => (Promise.reject('1')), () => (Promise.resolve(1)));
var pc9 = p.then(() => (Promise.reject('1')), () => (Promise.reject(1)));