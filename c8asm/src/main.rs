use clap::Parser;
use regex::Regex;
use std::error::Error;
use std::fs;
use std::io;

#[derive(Parser)]
struct Cli {
    #[clap(parse(from_os_str))]
    path: std::path::PathBuf,
}

#[derive(Debug)]
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
}

fn err(s: &str) -> io::Error {
    io::Error::new(io::ErrorKind::Other, s.to_string())
}

fn boxed_err(s: &str) -> Box<dyn Error> {
    Box::new(err(s))
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
        }
    }

    pub fn to_u16(self) -> u16 {
        match self {
            Token::ClearScreen => 0b00000000_11100000,
            Token::SetDigit {
                digit
            } => 0b1111_0000_00101001 | ((digit as u16) << 8),
            Token::Draw { register_x , register_y , height } =>
               0b1101_0000_0000_0000 | (register_x as u16) << 8 | (register_y as u16) << 4 | (height as u16),
        }
    }

    pub fn of_string(line: &str) -> Result<Self, Box<dyn Error>> {
        let line_regex = Regex::new(r"^ *([a-zA-Z_-]+) *(.*) *$").unwrap();

        let parts = line_regex
            .captures(line.trim())
            .ok_or(err("does not look like a command"))?;

        let command = parts.get(1).ok_or(err("no command part"))?.as_str();
        let arguments: Vec<&str> = parts
            .get(2)
            .ok_or(err("no arguments part)"))?
            .as_str()
            .split(",")
            .collect();

        println!("{:?}", command);

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

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let contents: String = fs::read_to_string(args.path)?.parse()?;

    for line in contents.lines() {
        let next_token = Token::of_string(line)?;
        println!("{:#02x?}", next_token.to_u16().to_be_bytes());
    }

    Ok(())
}
