#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        let z = match self {
          Command::Power(true, num) => "Power increased by ".to_string() + &num.to_string() + &"%".to_string(),
          Command::Power(false, num) => "Power decreased by ".to_string() + &num.to_string() + &"%".to_string(),
          Command::Missiles(true, num) => "Missiles increased by ".to_string() + &num.to_string(),
          Command::Missiles(false, num) => "Missiles decreased by ".to_string() + &num.to_string(),
          Command::Shield(true) => "Shield turned on".to_string(),
          Command::Shield(false) => "Shield turned off".to_string(),
          Command::Try => "Call attempt failed".to_string(),
          Command::Invalid => "Not a command".to_string(),
        };
        return z;
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    let commandV = s.split_ascii_whitespace().collect::<Vec<&str>>();
    if commandV.len() == 3 {
      if commandV[0] == "power" && commandV[1]=="inc" {
        match commandV[2].parse::<i32>() {
              Ok(n) => {
                return Command::Power(true, n);
              }
              Err(e) => {
                return Command::Invalid;
              }
            }
      } else if commandV[0] == "power" && commandV[1]=="dec" {
        match commandV[2].parse::<i32>() {
              Ok(n) => {
                return Command::Power(false, n);
              }
              Err(e) => {
                return Command::Invalid;
              }
          }            
      } else if commandV[0] == "add" && commandV[2]=="missiles" {
        match commandV[1].parse::<i32>() {
              Ok(n) => {
                return Command::Missiles(true, n);
              }
              Err(e) => {
                return Command::Invalid;
              }
            }            
      } else if commandV[0] == "fire" && commandV[2]=="missiles" {
        match commandV[1].parse::<i32>() {
              Ok(n) => {
                return Command::Missiles(false, n);
              }
              Err(e) => {
                return Command::Invalid;
              }
            }            
      }
       else {
        return Command::Invalid;
      }
    } else if commandV.len() == 2  {
      if commandV[0] == "shield" && commandV[1] == "on" {
        return Command::Shield(true);
      } else if commandV[0] == "shield" && commandV[1] == "off" {
        return Command::Shield(false);
      } else {
        return Command::Invalid;
      }
    }
    else if s == "try calling Miss Potts" {
      return Command::Try;
    } else {
      return Command::Invalid;
    }
}
