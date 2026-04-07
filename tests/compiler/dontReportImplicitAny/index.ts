//@compiler-options: noImplicitAny=false

type T18 = Awaited<{ then(cb: (value: number, other: { }) => void)}>; // number