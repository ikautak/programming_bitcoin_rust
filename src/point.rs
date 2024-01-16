use num_traits::Num;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct Point<T>
where
    T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + PartialEq,
{
    pub x: Option<T>,
    pub y: Option<T>,
    pub a: T,
    pub b: T,
}

impl<T> Point<T>
where
    T: Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + PartialEq
        + std::fmt::Debug
        + Copy,
{
    pub fn new(x: Option<T>, y: Option<T>, a: T, b: T) -> Self {
        if x.is_none() && y.is_none() {
            // Infinity
            return Self {
                x: None,
                y: None,
                a,
                b,
            };
        }

        let x = x.unwrap();
        let y = y.unwrap();

        if y * y != x * x * x + a * x + b {
            panic!("{:?} {:?} is not on the curve", x, y);
        }

        Self {
            x: Some(x),
            y: Some(y),
            a,
            b,
        }
    }
}

impl<T> PartialEq for Point<T>
where
    T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + PartialEq,
{
    fn eq(&self, rhs: &Self) -> bool {
        return self.x == rhs.x && self.y == rhs.y && self.a == rhs.a && self.b == rhs.b;
    }
}

impl<T> Add for Point<T>
where
    T: Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + PartialEq
        + std::fmt::Debug
        + Copy,
{
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        if self.a != rhs.a || self.b != rhs.b {
            panic!("Points {:?} {:?} are not on the same curve", self.a, self.b);
        }

        if self.x.is_none() {
            return rhs;
        }
        if rhs.x.is_none() {
            return self;
        }

        let x0 = self.x.unwrap();
        let x1 = rhs.x.unwrap();
        let y0 = self.y.unwrap();
        let y1 = rhs.y.unwrap();

        // Case 1: x is equal and y is not equal -> Infinity
        if x0 == x1 && y0 != y1 {
            return Self {
                x: None,
                y: None,
                a: self.a,
                b: self.b,
            };
        }

        // Case 2: x is not equal
        if x0 != x1 {
            let s = (y1 - y0) / (x1 - x0);
            let x = s * s - x0 - x1;
            let y = s * (x0 - x) - y0;
            return Self {
                x: Some(x),
                y: Some(y),
                a: self.a,
                b: self.b,
            };
        }

        // Case 4: tangent to vertical line -> Infinity
        if self == rhs && y0 == (y1 - y1) {
            return Self {
                x: None,
                y: None,
                a: self.a,
                b: self.b,
            };
        }

        // Case 3: self == other
        let one = x0 / x0;
        let two = one + one;
        let three = two + one;
        let s = (three * x0 * x0 + self.a) / (two * y0);
        let x = s * s - two * x0;
        let y = s * (x0 - x) - y0;
        Self {
            x: Some(x),
            y: Some(y),
            a: self.a,
            b: self.b,
        }
    }
}

/// scalar multiplications
impl<T, U> Mul<U> for Point<T>
where
    T: Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + PartialEq
        + std::fmt::Debug
        + Copy,
    U: Num + PartialOrd + Copy,
{
    type Output = Point<T>;
    fn mul(self, coefficient: U) -> Point<T> {
        let mut coef = coefficient;
        let mut current = self;
        let mut result = Point::new(None, None, self.a, self.b);

        let zero = U::zero();
        let one = U::one();
        let two = one + one;
        while coef > zero {
            if coef % two > zero {
                result = result + current;
            }
            current = current + current;
            coef = coef / two;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::Point;

    #[test]
    fn test_point_ne() {
        let a = Point::new(Some(3), Some(-7), 5, 7);
        let b = Point::new(Some(18), Some(77), 5, 7);
        assert!(a != b);
    }

    #[test]
    fn test_point_on_curve() {
        let _a = Point::new(Some(3), Some(-7), 5, 7);
        let _b = Point::new(Some(18), Some(77), 5, 7);
    }

    #[test]
    #[should_panic]
    fn test_point_on_curve_panic() {
        let _a = Point::new(Some(-2), Some(4), 5, 7);
    }

    #[test]
    fn test_point_add0() {
        let a = Point::new(None, None, 5, 7);
        let b = Point::new(Some(2), Some(5), 5, 7);
        let c = Point::new(Some(2), Some(-5), 5, 7);
        assert_eq!(a + b, b);
        assert_eq!(b + a, b);
        assert_eq!(b + c, a);
    }

    #[test]
    fn test_point_add1() {
        let a = Point::new(Some(3), Some(7), 5, 7);
        let b = Point::new(Some(-1), Some(-1), 5, 7);
        let c = Point::new(Some(2), Some(-5), 5, 7);
        assert_eq!(a + b, c);
    }

    #[test]
    fn test_point_add2() {
        let a = Point::new(Some(-1), Some(1), 5, 7);
        let b = Point::new(Some(18), Some(-77), 5, 7);
        assert_eq!(a + a, b);
    }
}
