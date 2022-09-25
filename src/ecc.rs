use num_traits::Num;
use once_cell::sync::Lazy;
use primitive_types::U512;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct FieldElement<T>
where
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    pub num: T,
    pub prime: T,
}

impl<T> FieldElement<T>
where
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    pub fn new(num: T, prime: T) -> Self {
        if num >= prime {
            panic!("Num {:?} not in field range 0 to {:?}", num, prime);
        }

        Self { num, prime }
    }

    pub fn pow(&mut self, mut exp: T) {
        let one = T::one();
        let mut result = FieldElement::new(one, self.prime);

        exp = exp % (self.prime - one);

        while exp > T::zero() {
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
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    fn eq(&self, rhs: &Self) -> bool {
        return self.prime == rhs.prime && self.num == rhs.num;
    }
}

impl<T> Add for FieldElement<T>
where
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
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
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
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
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
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
    T: Num + std::fmt::Debug + std::cmp::PartialOrd + Clone + Copy,
{
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        let mut other = rhs;
        other.pow(self.prime - T::one() - T::one());
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
#[derive(Debug)]
pub struct S256Point {
    point: Point<S256Field>,
}

impl S256Point {
    pub fn new(x: U512, y: U512) -> Self {
        let a = S256Field::new(*A, *P);
        let b = S256Field::new(*B, *P);
        let x = S256Field::new(x, *P);
        let y = S256Field::new(y, *P);

        Self {
            point: Point::<S256Field>::new(Some(x), Some(y), a, b),
        }
    }

    pub fn rmul(&self, coefficient: U512) -> Self {
        let coef = coefficient % *N;
        Self {
            point: self.point * coef,
        }
    }
}

impl PartialEq for S256Point {
    fn eq(&self, rhs: &Self) -> bool {
        return self.point == rhs.point;
    }
}

static GX: Lazy<U512> = Lazy::new(|| {
    U512::from_str_radix(
        "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
        16,
    )
    .unwrap()
});
static GY: Lazy<U512> = Lazy::new(|| {
    U512::from_str_radix(
        "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8",
        16,
    )
    .unwrap()
});
static G: Lazy<S256Point> = Lazy::new(|| S256Point::new(*GX, *GY));
#[cfg(test)]
mod tests {
    use super::FieldElement;
    use super::Point;
    use super::S256Point;
    use super::G;
    use super::N;
    use super::U512;

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

    #[test]
    fn test_s256_order() {
        let p = G.rmul(*N);
        assert!(p.point.x == None);
    }

    #[test]
    fn test_s256_pubpoint() {
        // write a test that tests the public point for the following
        let points = vec![
            // secret, x, y
            (
                U512::from(7i32),
                U512::from_str_radix(
                    "5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc",
                    16,
                )
                .unwrap(),
                U512::from_str_radix(
                    "6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da",
                    16,
                )
                .unwrap(),
            ),
            (
                U512::from(1485i32),
                U512::from_str_radix(
                    "c982196a7466fbbbb0e27a940b6af926c1a74d5ad07128c82824a11b5398afda",
                    16,
                )
                .unwrap(),
                U512::from_str_radix(
                    "7a91f9eae64438afb9ce6448a1c133db2d8fb9254e4546b6f001637d50901f55",
                    16,
                )
                .unwrap(),
            ),
            (
                U512::from_str_radix("100000000000000000000000000000000", 16).unwrap(),
                U512::from_str_radix(
                    "8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da",
                    16,
                )
                .unwrap(),
                U512::from_str_radix(
                    "662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82",
                    16,
                )
                .unwrap(),
            ),
            (
                U512::from_str_radix(
                    "1000000000000000000000000000000000000000000000000000080000000",
                    16,
                )
                .unwrap(),
                U512::from_str_radix(
                    "9577ff57c8234558f293df502ca4f09cbc65a6572c842b39b366f21717945116",
                    16,
                )
                .unwrap(),
                U512::from_str_radix(
                    "10b49c67fa9365ad7b90dab070be339a1daf9052373ec30ffae4f72d5e66d053",
                    16,
                )
                .unwrap(),
            ),
        ];

        for (secret, x, y) in points {
            let point = S256Point::new(x, y);
            let res = G.rmul(secret);
            assert_eq!(res, point);
        }
    }
}
