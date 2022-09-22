use num_traits::NumAssign;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    pub num: T,
    pub prime: T,
}

impl<T> FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    pub fn new(num: T, prime: T) -> Self {
        if num >= prime {
            panic!(
                "Num {:?} not in field range 0 to {:?}",
                num,
                prime - T::one()
            );
        }

        Self { num, prime }
    }

    pub fn pow(&mut self, mut exp: T) {
        let zero = T::zero();
        let one = T::one();
        let mut result = FieldElement::new(one, self.prime);

        exp = exp % (self.prime - one);

        while exp > zero {
            if exp % (one + one) == one {
                result = result * *self;
            }
            *self = *self * *self;
            exp /= one + one;
        }
        *self = result
    }
}

impl<T> PartialEq for FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    fn eq(&self, rhs: &Self) -> bool {
        return self.prime == rhs.prime && self.num == rhs.num;
    }
}

impl<T> Add for FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        Self {
            num: (self.num + rhs.num) % self.prime,
            prime: self.prime,
        }
    }
}

impl<T> Sub for FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        let num = if self.num >= rhs.num {
            self.num - rhs.num
        } else {
            self.prime - (rhs.num - self.num)
        };
        Self {
            num,
            prime: self.prime,
        }
    }
}

impl<T> Mul for FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        Self {
            num: (self.num * rhs.num) % self.prime,
            prime: self.prime,
        }
    }
}

impl<T> Div for FieldElement<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        let mut other = rhs.clone();
        let one = T::one();
        other.pow(self.prime - one - one);
        self * other
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Point<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd,
{
    pub x: Option<T>,
    pub y: Option<T>,
    pub a: T,
    pub b: T,
}

impl<T> Point<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Copy,
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
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd,
{
    fn eq(&self, rhs: &Self) -> bool {
        return self.x == rhs.x && self.y == rhs.y && self.a == rhs.a && self.b == rhs.b;
    }
}

impl<T> Add for Point<T>
where
    T: NumAssign + std::fmt::Debug + std::cmp::PartialOrd + Copy,
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

#[cfg(test)]
mod tests {
    use super::FieldElement;
    use super::Point;

    #[test]
    fn test_fe_ne() {
        let a = FieldElement::new(2i32, 31i32);
        let b = FieldElement::new(2i32, 31i32);
        let c = FieldElement::new(17i32, 31i32);
        assert_eq!(a, b);
        assert!(a != c);
        assert!(b != c);
    }

    #[test]
    fn test_fe_add() {
        let a = FieldElement::new(2i32, 31i32);
        let b = FieldElement::new(15i32, 31i32);
        let c = FieldElement::new(17i32, 31i32);
        assert_eq!(a + b, c);

        let a = FieldElement::new(17i32, 31i32);
        let b = FieldElement::new(21i32, 31i32);
        let c = FieldElement::new(7i32, 31i32);
        assert_eq!(a + b, c);
    }

    #[test]
    fn test_fe_sub() {
        let a = FieldElement::new(29i32, 31i32);
        let b = FieldElement::new(4i32, 31i32);
        let c = FieldElement::new(25i32, 31i32);
        assert_eq!(a - b, c);

        let a = FieldElement::new(15i32, 31i32);
        let b = FieldElement::new(30i32, 31i32);
        let c = FieldElement::new(16i32, 31i32);
        assert_eq!(a - b, c);
    }

    #[test]
    fn test_fe_mul() {
        let a = FieldElement::new(24i32, 31i32);
        let b = FieldElement::new(19i32, 31i32);
        let c = FieldElement::new(22i32, 31i32);
        assert_eq!(a * b, c);
    }

    #[test]
    fn test_fe_pow() {
        let mut a = FieldElement::new(17i32, 31i32);
        let b = FieldElement::new(15i32, 31i32);
        a.pow(3i32);
        assert_eq!(a, b);

        let mut a = FieldElement::new(5i32, 31i32);
        let b = FieldElement::new(18i32, 31i32);
        let c = FieldElement::new(16i32, 31i32);
        a.pow(5i32);
        assert_eq!(a * b, c);
    }

    #[test]
    fn test_fe_div() {
        let a = FieldElement::new(3i32, 31i32);
        let b = FieldElement::new(24i32, 31i32);
        let c = FieldElement::new(4i32, 31i32);
        assert_eq!(a / b, c);
        let a = FieldElement::new(1i32, 31i32);
        let b = FieldElement::new(17i32, 31i32);
        let c = FieldElement::new(29i32, 31i32);
        assert_eq!(a / b / b / b, c);
    }

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
