f((v) => (v ? [0] : Promise.reject()));
f(async (v) => (v ? [0] : Promise.reject()));
g((v) => (v ? 'contextuallyTypable' : Promise.reject()));
g(async (v) => (v ? 'contextuallyTypable' : Promise.reject()));
h((v) => (v ? (abc) => {} : Promise.reject()));
h(async (v) => (v ? (def) => {} : Promise.reject()));
var increment = async (num, str) => ((a) => (a.length));
var increment2 = async (num, str) => ((a) => (a.length));