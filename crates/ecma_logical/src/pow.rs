pub fn pow(x: f64, y: f64) -> f64 {
    if y.is_nan() || x.is_nan() || (y.is_infinite() && (x == 1.0 || x == -1.0)) {
        f64::NAN
    } else if y == 2.0 {
        x * x
    } else if y == 0.5 {
        if x.is_infinite() {
            f64::INFINITY
        } else {
            x.sqrt()
        }
    } else {
        x.powf(y)
    }
}
