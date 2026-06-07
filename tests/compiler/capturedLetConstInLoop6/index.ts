// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/capturedLetConstInLoop5_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// ====let
for (let x of []) {
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

for (let x in []) {
    (function() { return x});
    (() => x);
    if (x == "1") {
        break;
    }
    if (x == "2") {
        continue;
    }
}


for (let x = 0; x < 1; ++x) {
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

while (1 === 1) {
    let x;
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

do {
    let x;
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
} while (1 === 1)

for (let y = 0; y < 1; ++y) {
    let x = 1;
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

for (let x = 0, y = 1; x < 1; ++x) {
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

while (1 === 1) {
    let x, y;
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

do {
    let x, y;
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
} while (1 === 1)

for (let y = 0; y < 1; ++y) {
    let x = 1;
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

// ====const

for (const x of []) {
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

for (const x in []) {
    (function() { return x});
    (() => x);
    if (x == "1") {
        break;
    }
    if (x == "2") {
        continue;
    }
}


for (const x = 0; x < 1;) {
    (function() { return x});
    (() => x);
    if (x == 1) {
      //~^ ERROR: This comparison appears to be unintentional because the types '0' and '1' have no overlap.
        break;
    }
    if (x == 2) {
      //~^ ERROR: This comparison appears to be unintentional because the types '0' and '2' have no overlap.
        continue;
    }
}

while (1 === 1) {
    const x = 1;
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

do {
    const x = 1;
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
} while (1 === 1)

for (const y = 0; y < 1;) {
    const x = 1;
    (function() { return x});
    (() => x);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

for (const x = 0, y = 1; x < 1;) {
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
      //~^ ERROR: This comparison appears to be unintentional because the types '0' and '1' have no overlap.
        break;
    }
    if (x == 2) {
      //~^ ERROR: This comparison appears to be unintentional because the types '0' and '2' have no overlap.
        continue;
    }
}

while (1 === 1) {
    const x = 1, y = 1;
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

do {
    const x = 1, y = 1;
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
} while (1 === 1)

for (const y = 0; y < 1;) {
    const x = 1;
    (function() { return x + y});
    (() => x + y);
    if (x == 1) {
        break;
    }
    if (x == 2) {
        continue;
    }
}

