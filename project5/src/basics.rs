/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1;
      } else {
        let mut res = 0;
        let mut i = 0;
        while i <= n {
            res = res + i;
            i = i + 1;
        }
        return res;
    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut res = 0;
    for el in ls.iter() {
        if el >= &s && el <= &e {
        res=res+1;
        }
    }
    return res;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for el in target.iter() {
        if !set.contains(&el) {
          return false;
        }
      }
      return true;
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.len() == 0 {
        return None;
    } else {
        let mut sum = 0.0;
        let x = ls.len() as f64;
        for el in ls.iter() {
            sum = sum + el;
        }
        return Some(sum / x);
    }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut res = 0;
    if ls.len() == 0 {
      return 0;
    }
    let mut i = (ls.len()-1) as i32;
    while i >= 0 {
      let el = ls[i as usize];
      let expo = (ls.len() as u32)-((i+1) as u32);
      res = res + el*((2 as i32).pow(expo));
      i = i-1;
    }
    return res;
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn find_factors(n:u32) -> Vec<u32> {
    for i in 2..n {
      if n % i == 0 {
        return vec![i, n / i];
      }
    }
    return vec![1,1];
  }

  pub fn factorize(n: u32) -> Vec<u32> {
      let factors = find_factors(n);
      if factors == [1,1] {
        return vec![n];
      } else {
        let mut first_factor = factorize(factors[0]);
        let second_factor = factorize(factors[1]);
        first_factor.extend(second_factor);
        return first_factor;
      }
  }

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {

    let mut vec = Vec::new();
    if lst.len() == 0 {
      return vec;
    }
    for i in 1..lst.len() {
      vec.push(lst[i]);
    }
    vec.push(lst[0]);

    return vec;
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let mut i = 0;
    if target.len() > s.len() {
      return false;
    }
    let end = s.len()-target.len()+1;
    while i < end {
      let sub = &s[i..i+target.len()];
      if sub == target {
        return true;
      }
      i = i + 1;
    }
    return false;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.len() == 0 {
        return None;
      } else {
        let mut start = 0;
        let mut end = 1;
        let mut max_start = 0;
        let mut max_end = 0;
        let mut curr_char = &s[0..1];

        while end < s.len() {
          let ch = &s[end..end+1];
          if ch == curr_char {
            if end-start > max_end-max_start {
              max_start = start;
              max_end = end;
            }
            end = end + 1;
          } else {
            start = end;
            end = end + 1;
            curr_char = ch;
          }
          }
        
        return Some(&s[max_start..max_end+1]);
    }
}
