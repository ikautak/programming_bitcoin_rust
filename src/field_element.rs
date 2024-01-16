use num_traits::Num;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Rem, Sub};

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
    T: Num
        + std::fmt::Debug
        + std::cmp::PartialOrd
        + Clone
        + Copy
        + Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + Rem<Output = T>,
{
    pub fn new(num: T, prime: T) -> Self {
        if num >= prime {
            panic!("Num {:?} not in field range 0 to {:?}", num, prime);
        }

        Self { num, prime }
    }

    pub fn pow(self, exp: T) -> Self {
        let one = T::one();
        let zero = T::zero();

        if exp < zero {
            // Handle negative exponent if needed
            // ...
        }

        let mut exp = exp % (self.prime - one);
        let mut result = FieldElement::new(one, self.prime);
        let mut base = self;

        while exp > zero {
            if exp % (one + one) == one {
                result = result * base;
            }
            base = base * base;
            exp = exp / (one + one);
        }

        result
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

        let p = rhs.pow(self.prime - T::one() - T::one());
        self * p
    }
}

#[cfg(test)]
mod tests {
    use super::FieldElement;

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
        let a = FieldElement::new(17i32, 31i32);
        let b = FieldElement::new(15i32, 31i32);
        let a_pow = a.pow(3i32);
        assert_eq!(a_pow, b);

        let a = FieldElement::new(5i32, 31i32);
        let b = FieldElement::new(18i32, 31i32);
        let c = FieldElement::new(16i32, 31i32);
        let a_pow = a.pow(5i32);
        assert_eq!(a_pow * b, c);
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
}
