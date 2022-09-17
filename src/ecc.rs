use primitive_types::{U256, U512};
use std::cmp::PartialEq;
use std::convert::TryFrom;
use std::ops::{Add, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct FieldElement {
    pub num: U256,
    pub prime: U256,
}

impl FieldElement {
    pub fn new(num: U256, prime: U256) -> Self {
        if num >= prime {
            panic!("Num {} not in field range 0 to {}", num, prime - 1);
        }

        Self { num, prime }
    }

    pub fn pow(&mut self, mut exp: u64) {
        let mut result = FieldElement::new(U256::from(1u64), self.prime);
        while exp > 0 {
            if exp & 1 == 1 {
                result = result * *self;
                println!("result {:?}", self);
            }
            *self = *self * *self;
            println!("self {:?}", self);
            exp /= 2;
        }
        *self = result
    }
}

impl PartialEq for FieldElement {
    fn eq(&self, rhs: &Self) -> bool {
        return self.prime == rhs.prime && self.num == rhs.num;
    }
}

impl Add for FieldElement {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        let num = (U512::from(self.num) + U512::from(rhs.num)) % self.prime;
        let num = U256::try_from(num).unwrap();

        Self {
            num,
            prime: self.prime,
        }
    }
}

impl Sub for FieldElement {
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

impl Mul for FieldElement {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        if self.prime != rhs.prime {
            panic!("Cannot ad two numbers in different Fields");
        }

        let num = (U512::from(self.num) * U512::from(rhs.num)) % self.prime;
        let num = U256::try_from(num).unwrap();
        Self {
            num,
            prime: self.prime,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::FieldElement;
    use primitive_types::U256;

    #[test]
    fn test_ne() {
        let a = FieldElement::new(U256::from(2u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(2u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(17u32), U256::from(31u32));
        assert_eq!(a, b);
        assert!(a != c);
        assert!(b != c);
    }

    #[test]
    fn test_add() {
        let a = FieldElement::new(U256::from(2u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(15u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(17u32), U256::from(31u32));
        assert_eq!(a + b, c);

        let a = FieldElement::new(U256::from(17u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(21u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(7u32), U256::from(31u32));
        assert_eq!(a + b, c);
    }

    #[test]
    fn test_sub() {
        let a = FieldElement::new(U256::from(29u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(4u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(25u32), U256::from(31u32));
        assert_eq!(a - b, c);

        let a = FieldElement::new(U256::from(15u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(30u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(16u32), U256::from(31u32));
        assert_eq!(a - b, c);
    }

    #[test]
    fn test_mul() {
        let a = FieldElement::new(U256::from(24u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(19u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(22u32), U256::from(31u32));
        assert_eq!(a * b, c);
    }

    #[test]
    fn test_pow() {
        let mut a = FieldElement::new(U256::from(17u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(15u32), U256::from(31u32));
        a.pow(3);
        assert_eq!(a, b);

        let mut a = FieldElement::new(U256::from(5u32), U256::from(31u32));
        let b = FieldElement::new(U256::from(18u32), U256::from(31u32));
        let c = FieldElement::new(U256::from(16u32), U256::from(31u32));
        a.pow(5);
        assert_eq!(a * b, c);
    }
}
