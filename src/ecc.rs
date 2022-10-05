//! programming-bitcoin
//! ecc.py
use num_traits::Num;
use once_cell::sync::Lazy;
use primitive_types::U512;
use rand::Rng;
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

/// It's actually 256bit data, but use U512 for simplify calculation.
type S256Field = FieldElement<U512>;

impl S256Field {
    pub fn sqrt(&self) -> Self {
        let p = (*P + U512::one()) / U512::from(4u32);
        let mut ret = self.clone();
        ret.pow(p);
        ret
    }
}

#[derive(Debug)]
pub struct S256Point {
    pub point: Point<S256Field>,
}

/// x to the power exp, modulo m
pub fn modpow<T>(mut x: T, mut exp: T, m: T) -> T
where
    T: Num + PartialOrd + Copy,
{
    let one = T::one();
    let mut result = T::one();

    while exp > T::zero() {
        if exp % (one + one) == one {
            result = result * x % m;
        }
        x = x * x % m;
        exp = exp / (one + one);
    }

    result
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

    pub fn mul(&self, coefficient: U512) -> Self {
        let coef = coefficient % *N;
        Self {
            point: self.point * coef,
        }
    }

    pub fn verify(&self, z: U512, sig: Signature) -> bool {
        let n = *N;
        let s_inv = modpow(sig.s, n - 2, n);

        let u = z * s_inv % n;
        let v = sig.r * s_inv % n;
        let total = G.mul(u).point + self.mul(v).point;
        total.x.unwrap().num == sig.r
    }

    pub fn sec(&self, compressed: bool) -> Vec<u8> {
        let mut out = Vec::<u8>::new();

        if compressed {
            if self.point.y.unwrap().num % 2 == U512::zero() {
                out.push(0x02u8);
            } else {
                out.push(0x03u8);
            }

            let mut big_endian_bytes = [0u8; 64];
            self.point
                .x
                .unwrap()
                .num
                .to_big_endian(&mut big_endian_bytes);

            for i in 32..64 {
                out.push(big_endian_bytes[i]);
            }

            out
        } else {
            let mut big_endian_bytes_x = [0u8; 64];
            let mut big_endian_bytes_y = [0u8; 64];
            self.point
                .x
                .unwrap()
                .num
                .to_big_endian(&mut big_endian_bytes_x);
            self.point
                .y
                .unwrap()
                .num
                .to_big_endian(&mut big_endian_bytes_y);
            println!("{:?}", big_endian_bytes_x);

            out.push(0x04u8);

            for i in 32..64 {
                out.push(big_endian_bytes_x[i]);
            }
            for i in 32..64 {
                out.push(big_endian_bytes_y[i]);
            }

            out
        }
    }

    pub fn parse(sec_bin: &str) -> Self {
        let sec_bin = hex::decode(sec_bin).unwrap();

        if sec_bin[0] == 0x4 {
            // uncompressed
            let x = U512::from(sec_bin[1..33].as_ref());
            let y = U512::from(sec_bin[33..65].as_ref());
            return S256Point::new(x, y);
        }

        let is_even = sec_bin[0] == 0x2;
        let mut x = S256Field::new(U512::from_big_endian(sec_bin[1..33].as_ref()), *P);
        x.pow(U512::from(3u32));
        let alpha = x + S256Field::new(U512::from(8u32), *P);
        let beta = alpha.sqrt();

        let even_beta: FieldElement<U512>;
        let odd_beta: FieldElement<U512>;
        if beta.num % U512::from(2u32) == U512::zero() {
            even_beta = beta.clone();
            odd_beta = S256Field::new(*P - beta.num, *P);
        } else {
            even_beta = S256Field::new(*P - beta.num, *P);
            odd_beta = beta.clone();
        }

        if is_even {
            Self::new(x.num, even_beta.num)
        } else {
            Self::new(x.num, odd_beta.num)
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

#[derive(Debug, Copy, Clone)]
pub struct Signature {
    r: U512,
    s: U512,
}

impl Signature {
    pub fn new(r: U512, s: U512) -> Self {
        Self { r, s }
    }
}

pub struct PrivateKey {
    pub secret: U512,
    pub point: S256Point,
}

impl PrivateKey {
    pub fn new(z: U512) -> Self {
        Self {
            secret: z,
            point: G.mul(z),
        }
    }

    pub fn sign(&self, z: U512) -> Signature {
        let k: u32 = rand::thread_rng().gen(); // FIXME
        let k = U512::from(k);
        let r = G.mul(k).point.x.unwrap().num;
        let n = *N;
        let k_inv = modpow(k, n - 2, n);
        let mut s = (z + r * self.secret % n) * k_inv % n;
        if s > n / 2 {
            s = n - s;
        }
        Signature { r, s }
    }
}

#[cfg(test)]
mod tests {
    use super::FieldElement;
    use super::Point;
    use super::PrivateKey;
    use super::Rng;
    use super::S256Point;
    use super::Signature;
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
        let p = G.mul(*N);
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
            let res = G.mul(secret);
            assert_eq!(res, point);
        }
    }

    #[test]
    fn test_s256_verify() {
        let p = S256Point::new(
            U512::from_str_radix(
                "887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c",
                16,
            )
            .unwrap(),
            U512::from_str_radix(
                "61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34",
                16,
            )
            .unwrap(),
        );

        let z = U512::from_str_radix(
            "ec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60",
            16,
        )
        .unwrap();
        let r = U512::from_str_radix(
            "ac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395",
            16,
        )
        .unwrap();
        let s = U512::from_str_radix(
            "68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4",
            16,
        )
        .unwrap();
        let sig = Signature::new(r, s);
        assert!(p.verify(z, sig));

        let z = U512::from_str_radix(
            "7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d",
            16,
        )
        .unwrap();
        let r = U512::from_str_radix(
            "eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c",
            16,
        )
        .unwrap();
        let s = U512::from_str_radix(
            "c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6",
            16,
        )
        .unwrap();
        let sig = Signature::new(r, s);
        assert!(p.verify(z, sig));
    }

    #[test]
    fn test_s256_sec() {
        // case 1
        let coefficient = U512::from(999u32).pow(U512::from(3u32));
        let point = G.mul(coefficient);

        let sec = point.sec(false);
        let uncompressed = "049d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9";
        let uncompressed = hex::decode(uncompressed).unwrap();
        assert_eq!(sec, uncompressed);

        let sec = point.sec(true);
        let compressed = "039d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5";
        let compressed = hex::decode(compressed).unwrap();
        assert_eq!(sec, compressed);

        // case 2
        let coefficient = U512::from(123u32);
        let point = G.mul(coefficient);

        let sec = point.sec(false);
        let uncompressed = "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b";
        let uncompressed = hex::decode(uncompressed).unwrap();
        assert_eq!(sec, uncompressed);

        let sec = point.sec(true);
        let compressed = "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5";
        let compressed = hex::decode(compressed).unwrap();
        assert_eq!(sec, compressed);

        // case 3
        let coefficient = U512::from(42424242u32);
        let point = G.mul(coefficient);

        let sec = point.sec(false);
        let uncompressed = "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3";
        let uncompressed = hex::decode(uncompressed).unwrap();
        assert_eq!(sec, uncompressed);

        let sec = point.sec(true);
        let compressed = "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e";
        let compressed = hex::decode(compressed).unwrap();
        assert_eq!(sec, compressed);
    }

    #[test]
    fn test_sign() {
        let rnd: u32 = rand::thread_rng().gen();
        let pk = PrivateKey::new(U512::from(rnd));

        let rnd: u32 = rand::thread_rng().gen();
        let z = U512::from(rnd);
        let sig = pk.sign(z);
        assert!(pk.point.verify(z, sig));
    }
}
