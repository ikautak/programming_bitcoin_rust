use num_traits::{Num, NumOps};
use once_cell::sync::Lazy;
use primitive_types::U512;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct FieldElement<T>
where
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    pub num: T,
    pub prime: T,
}

impl<T> FieldElement<T>
where
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    pub fn new(num: T, prime: T) -> Self {
        if num >= prime {
            panic!("Num {:?} not in field range 0 to {:?}", num, prime);
        }

        Self { num, prime }
    }

    pub fn pow(&mut self, mut exp: T) {
        let zero = self.prime - self.prime;
        let one = self.prime / self.prime;
        let mut result = FieldElement::new(one, self.prime);

        exp = exp % (self.prime - one);

        while exp > zero {
            if exp % (one + one) == one {
                result = result * *self;
            }
            *self = *self * *self;
            exp = exp / (one + one);
        }
        *self = result
    }
}

impl<T> PartialEq for FieldElement<T>
where
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    fn eq(&self, rhs: &Self) -> bool {
        return self.prime == rhs.prime && self.num == rhs.num;
    }
}

impl<T> Add for FieldElement<T>
where
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
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
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
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
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
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
    T: NumOps + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        let mut other = rhs.clone();
        let one = self.prime / self.prime;
        other.pow(self.prime - one - one);
        self * other
    }
}

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

static A: Lazy<U512> = Lazy::new(|| U512::from(0));
static B: Lazy<U512> = Lazy::new(|| U512::from(7));
static P: Lazy<U512> =
    Lazy::new(|| U512::from(r"0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f"));
static N: Lazy<U512> =
    Lazy::new(|| U512::from(r"0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"));

type S256Field = FieldElement<U512>;
pub struct S256Point {
    point: Point<S256Field>,
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

    #[test]
    fn test_ecc_on_curve() {
        let prime = 223;
        let a = FieldElement::new(0, prime);
        let b = FieldElement::new(7, prime);

        let valid_points = vec![(192, 105), (17, 56), (1, 193)];

        for (x_raw, y_raw) in valid_points {
            let x = FieldElement::new(x_raw, prime);
            let y = FieldElement::new(y_raw, prime);
            let _p = Point::new(Some(x), Some(y), a, b);
        }
    }

    #[test]
    #[should_panic]
    fn test_ecc_on_curve_panic0() {
        let prime = 223;
        let a = FieldElement::new(0, prime);
        let b = FieldElement::new(7, prime);
        let x = FieldElement::new(200, prime);
        let y = FieldElement::new(119, prime);
        let _p = Point::new(Some(x), Some(y), a, b);
    }

    #[test]
    #[should_panic]
    fn test_ecc_on_curve_panic1() {
        let prime = 223;
        let a = FieldElement::new(0, prime);
        let b = FieldElement::new(7, prime);
        let x = FieldElement::new(42, prime);
        let y = FieldElement::new(99, prime);
        let _p = Point::new(Some(x), Some(y), a, b);
    }

    #[test]
    fn test_ecc_add() {
        let prime = 223i32;
        let a = FieldElement::new(0, prime);
        let b = FieldElement::new(7, prime);

        let additions = vec![
            // x1, y1, x2, y2, x3, y3
            (192, 105, 17, 56, 170, 142),
            (47, 71, 117, 141, 60, 139),
            (143, 98, 76, 66, 47, 71),
        ];

        for (x1, y1, x2, y2, x3, y3) in additions {
            let x1 = FieldElement::new(x1, prime);
            let y1 = FieldElement::new(y1, prime);
            let p1 = Point::new(Some(x1), Some(y1), a, b);

            let x2 = FieldElement::new(x2, prime);
            let y2 = FieldElement::new(y2, prime);
            let p2 = Point::new(Some(x2), Some(y2), a, b);

            let x3 = FieldElement::new(x3, prime);
            let y3 = FieldElement::new(y3, prime);
            let p3 = Point::new(Some(x3), Some(y3), a, b);
            assert_eq!(p1 + p2, p3);
        }
    }

    #[test]
    fn test_ecc_scalar_mul() {
        let prime = 223i32;
        let a = FieldElement::new(0, prime);
        let b = FieldElement::new(7, prime);

        // (coefficient, x1, y1, x2, y2)
        let multiplications = vec![
            (2, 192, 105, 49, 71),
            (2, 143, 98, 64, 168),
            (2, 47, 71, 36, 111),
            (4, 47, 71, 194, 51),
            (8, 47, 71, 116, 55),
        ];

        for (s, x1, y1, x2, y2) in multiplications {
            let x1 = FieldElement::new(x1, prime);
            let y1 = FieldElement::new(y1, prime);
            let p1 = Point::new(Some(x1), Some(y1), a, b);

            let x2 = FieldElement::new(x2, prime);
            let y2 = FieldElement::new(y2, prime);
            let p2 = Point::new(Some(x2), Some(y2), a, b);

            assert_eq!(p1 * s, p2);
        }
    }

    #[test]
    fn test_ecc_scalar_mul_none() {
        let prime = 223i32;
        let a = FieldElement::new(0, prime);
        let b = FieldElement::new(7, prime);
        let s = 21;

        let x1 = FieldElement::new(47, prime);
        let y1 = FieldElement::new(71, prime);
        let p1 = Point::new(Some(x1), Some(y1), a, b);

        let p2 = Point::new(None, None, a, b);

        assert_eq!(p1 * s, p2);
    }
}
