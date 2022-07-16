use clap::Parser;
use regex::Regex;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::io;

/// Programs don't start at zero so we offset addresses by this value.
const ADDRESS_OFFSET: usize = 0x200;

fn err(s: &str) -> io::Error {
    io::Error::new(io::ErrorKind::Other, s.to_string())
}

fn boxed_err(s: &str) -> Box<dyn Error> {
    Box::new(err(s))
}

#[derive(Parser)]
struct Cli {
    #[clap(parse(from_os_str))]
    path: std::path::PathBuf,
}

#[derive(Debug, Clone)]
enum Token {
    ClearScreen,
    SetDigit {
        /// Should be between 0 and 9
        digit: usize,
    },
    Draw {
        register_x: usize,
        register_y: usize,
        height: usize,
    },
    LoadImmediate {
        register: usize,
        immediate: u8,
    },
    AddImmediate {
        register: usize,
        immediate: u8,
    },
    JumpImmediate {
        label: String,
    },
    CmpRegisterImmediateEqual {
        register: usize,
        immediate: u8,
    },
    CmpRegisterImmediateNotEqual {
        register: usize,
        immediate: u8,
    },
}

impl Token {
    pub fn name(&self) -> String {
        match self {
            Token::ClearScreen => format!("cls"),
            Token::Draw {
                register_x: _,
                register_y: _,
                height: _,
            } => format!("draw"),
            Token::SetDigit { digit: _ } => format!("set-digit"),
            Token::LoadImmediate {
                register: _,
                immediate: _,
            } => format!("ldi"),
            Token::AddImmediate {
                register: _,
                immediate: _,
            } => format!("addi"),
            Token::JumpImmediate { label: _ } => format!("jmp"),
            Token::CmpRegisterImmediateEqual {
                register: _,
                immediate: _,
            } => format!("skip-eq-i"),
            Token::CmpRegisterImmediateNotEqual {
                register: _,
                immediate: _,
            } => format!("skip-neq-i"),
        }
    }

    // TODO: By not taking &self we copy any labels for no reason.
    pub fn to_u16(self, labels: &HashMap<String, usize>) -> Result<u16, Box<dyn Error>> {
        Ok(match self {
            Token::ClearScreen => 0b00000000_11100000,
            Token::SetDigit { digit } => 0b1111_0000_00101001 | ((digit as u16) << 8),
            Token::Draw {
                register_x,
                register_y,
                height,
            } => {
                0b1101_0000_0000_0000
                    | (register_x as u16) << 8
                    | (register_y as u16) << 4
                    | (height as u16)
            }
            Token::LoadImmediate {
                register,
                immediate,
            } => 0b0110_0000_0000_0000 | (register as u16) << 8 | (immediate as u16),
            Token::AddImmediate {
                register,
                immediate,
            } => 0b0111_0000_0000_0000 | (register as u16) << 8 | (immediate as u16),
            Token::JumpImmediate { label } => {
                let label_address = labels
                    .get(&label)
                    .ok_or(err(&format!("{label} is not a valid label")))?;
                0b0001_0000_0000_0000 | ((label_address + ADDRESS_OFFSET) as u16)
            }
            Token::CmpRegisterImmediateEqual {
                register,
                immediate,
            } => 0b0011_0000_0000_0000 | (register as u16) << 8 | (immediate as u16),
            Token::CmpRegisterImmediateNotEqual {
                register,
                immediate,
            } => 0b0100_0000_0000_0000 | (register as u16) << 8 | (immediate as u16),
        })
    }

    pub fn of_string(line: &str) -> Result<Self, Box<dyn Error>> {
        let line_regex = Regex::new(r"^ *([a-zA-Z_-]+) *(.*) *$").unwrap();

        let parts = line_regex
            .captures(line.trim())
            .ok_or(err(&format!("{line} does not look like a command")))?;

        let command = parts.get(1).ok_or(err("no command part"))?.as_str();
        let arguments: Vec<&str> = parts
            .get(2)
            .ok_or(err("no arguments part)"))?
            .as_str()
            .split(",")
            .map(|x| x.trim())
            .collect();

        if command == Token::ClearScreen.name() {
            Ok(Token::ClearScreen)
        } else if command
            == (Token::SetDigit {
                /* The value of digit here is ignored */ digit: 0,
            })
            .name()
        {
            let digit = arguments
                .get(0)
                .ok_or(err("no digit supplied"))?
                .parse::<usize>()?;
            if digit <= 9 {
                Ok(Token::SetDigit { digit })
            } else {
                Err(boxed_err(&format!("{} is not in the range 0-9", digit)))
            }
        } else if command
            == (Token::LoadImmediate {
                register: 0,
                immediate: 0,
            })
            .name()
        {
            let register = arguments
                .get(0)
                .ok_or(err("register invalid"))?
                .parse::<usize>()?;
            let immediate = arguments
                .get(1)
                .ok_or(err("immediate invalid"))?
                .parse::<u8>()?;
            Ok(Token::LoadImmediate {
                register,
                immediate,
            })
        } else if command
            == (Token::CmpRegisterImmediateEqual {
                register: 0,
                immediate: 0,
            })
            .name()
        {
            let register = arguments
                .get(0)
                .ok_or(err("register invalid"))?
                .parse::<usize>()?;
            let immediate = arguments
                .get(1)
                .ok_or(err("immediate invalid"))?
                .parse::<u8>()?;
            Ok(Token::CmpRegisterImmediateEqual {
                register,
                immediate,
            })
        } else if command
            == (Token::CmpRegisterImmediateNotEqual {
                register: 0,
                immediate: 0,
            })
            .name()
        {
            let register = arguments
                .get(0)
                .ok_or(err("register invalid"))?
                .parse::<usize>()?;
            let immediate = arguments
                .get(1)
                .ok_or(err("immediate invalid"))?
                .parse::<u8>()?;
            Ok(Token::CmpRegisterImmediateNotEqual {
                register,
                immediate,
            })
        } else if command
            == (Token::AddImmediate {
                register: 0,
                immediate: 0,
            })
            .name()
        {
            let register = arguments
                .get(0)
                .ok_or(err("register x invalid"))?
                .parse::<usize>()?;
            let immediate = arguments
                .get(1)
                .ok_or(err("register y invalid"))?
                .parse::<u8>()?;
            Ok(Token::AddImmediate {
                register,
                immediate,
            })
        } else if command
            == (Token::JumpImmediate {
                label: "".to_string(),
            })
            .name()
        {
            let label = arguments.get(0).ok_or(err("label invalid"))?;
            Ok(Token::JumpImmediate {
                label: label.to_string(),
            })
        } else if command
            == (Token::Draw {
                register_x: 0,
                register_y: 0,
                height: 0,
            })
            .name()
        {
            let register_x = arguments
                .get(0)
                .ok_or(err("register x invalid"))?
                .parse::<usize>()?;
            let register_y = arguments
                .get(1)
                .ok_or(err("register y invalid"))?
                .parse::<usize>()?;
            let height = arguments
                .get(2)
                .ok_or(err("height invalid"))?
                .parse::<usize>()?;

            // TODO: check bounds of inputs

            Ok(Token::Draw {
                register_x,
                register_y,
                height,
            })
        } else {
            Err(boxed_err(&format!("Do not understand the line: {}", line)))
        }
    }
}

#[derive(Debug)]
enum Statement {
    Token(Token),
    Label(String),
}

impl Statement {
    fn try_label(line: &str) -> Result<Self, Box<dyn Error>> {
        let label_regex = Regex::new(r"^([a-zA-Z0-9_-]+):$").unwrap();
        Ok(Self::Label(
            label_regex
                .captures(line.trim())
                .ok_or(err(&format!("{line} is not a label")))?
                .get(1)
                .ok_or(err(&format!("{line} is not a label")))?
                .as_str()
                .to_string(),
        ))
    }

    pub fn of_string(line: &str) -> Result<Self, Box<dyn Error>> {
        if let Ok(label) = Self::try_label(line) {
            Ok(label)
        } else {
            Ok(Self::Token(Token::of_string(line)?))
        }
    }
}

#[derive(Debug)]
struct Statements {
    stmts: Vec<Statement>,
}

impl Statements {
    pub fn of_string(program: &str) -> Result<Self, Box<dyn Error>> {
        let statements: Result<Vec<Statement>, Box<dyn Error>> = program
            .lines()
            .map(|x| x.trim())
            .filter(|x| x.len() > 0)
            .map(|line| Statement::of_string(line))
            .collect();
        Ok(Self { stmts: statements? })
    }

    pub fn to_bytes(self) -> Result<Vec<u8>, Box<dyn Error>> {
        println!("{:?}", &self.stmts);

        let mut output_bytes = Vec::new();
        let mut labels = HashMap::new();

        for statement in self.stmts {
            match statement {
                Statement::Token(token) => {
                    for byte in token.to_u16(&labels)?.clone().to_be_bytes() {
                        output_bytes.push(byte);
                    }
                }
                Statement::Label(label) => {
                    labels.insert(label.to_string(), output_bytes.len());
                }
            }
        }

        Ok(output_bytes)
    }
}

fn print_ocaml_formatted_byte_list(bytes: &[u8]) {
    let mut first = true;
    print!("[ ");
    for byte in bytes {
        if !first {
            print!(" ; ");
        }
        print!("0x{:02X?}", byte);
        first = false;
    }
    print!(" ]\n");
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let contents: String = fs::read_to_string(args.path)?.parse()?;

    let program = Statements::of_string(&contents)?;
    let bytes = program.to_bytes()?;
    print_ocaml_formatted_byte_list(&bytes);
    Ok(())
}
