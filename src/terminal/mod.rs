// the parts related to raw mode are ripped from termios crate
// - extern signatures from https://github.com/dcuddeback/termios-rs/blob/master/src/ffi.rs
// - c struct from https://github.com/dcuddeback/termios-rs/blob/master/src/os/linux.rs
#![allow(non_camel_case_types)]

use std::{
    collections::VecDeque,
    io::{Read, StdinLock, StdoutLock, Write},
    os::fd::{AsRawFd, RawFd},
};

type c_int = i32;
type cc_t = u8;
type speed_t = u32;
type tcflag_t = u32;

// https://linux.die.net/man/3/tcsetattr
const OPOST: tcflag_t = 0o000001; // Enable implementation-defined output processing.
const ONLCR: tcflag_t = 0o000004; // Map NL to CR-NL on output.
const TCSANOW: c_int = 0; // the change occurs immediately.

const NCCS: usize = 32;
const HISTORY_SIZE: usize = 1024;
const PROMPT: &[u8] = b">> ";

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
#[repr(C)]
pub struct termios {
    c_iflag: tcflag_t,
    c_oflag: tcflag_t,
    c_cflag: tcflag_t,
    c_lflag: tcflag_t,
    c_line: cc_t,
    c_cc: [cc_t; NCCS],
    c_ispeed: speed_t,
    c_ospeed: speed_t,
}

extern "C" {
    fn cfmakeraw(termios_p: *mut termios);
    fn tcgetattr(fd: c_int, termios_p: *mut termios) -> c_int;
    fn tcsetattr(fd: c_int, optional_actions: c_int, termios_p: *const termios) -> c_int;
}

pub struct Terminal<'a> {
    stdin: StdinLock<'a>,
    stdout: StdoutLock<'a>,
    fd_in: RawFd,
    prev_cf: termios,
    history: VecDeque<String>,
    buf: [u8; 1],
}

impl Drop for Terminal<'_> {
    fn drop(&mut self) {
        unsafe {
            if tcsetattr(self.fd_in, TCSANOW, &self.prev_cf) > 0 {
                eprintln!("restore terminal failed, try typing 'reset' + Enter");
            }
        }
    }
}

impl Terminal<'_> {
    pub fn new() -> Self {
        let stdin = std::io::stdin().lock();
        let stdout = std::io::stdout().lock();
        let fd_in = stdin.as_raw_fd();
        let mut prev_cf = termios::default();
        let mut raw_cf = termios::default();
        unsafe {
            let get_errno = tcgetattr(fd_in, &mut prev_cf);
            if get_errno > 0 {
                panic!("getting terminal attr failed with errno {get_errno}")
            }

            cfmakeraw(&mut raw_cf);
            raw_cf.c_oflag |= ONLCR | OPOST;

            let set_errno = tcsetattr(fd_in, TCSANOW, &raw_cf);
            if set_errno > 0 {
                panic!("setting terminal to raw failed with errno {set_errno}")
            }
        }
        Self {
            stdin,
            stdout,
            fd_in,
            prev_cf,
            history: VecDeque::with_capacity(HISTORY_SIZE),
            buf: [0],
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let mut tmp = vec![];
        loop {
            self.stdin
                .read_exact(&mut self.buf)
                .expect("reading byte failed");
            tmp.push(self.buf[0]);
            if tmp.len() > 4 {
                return None;
            }
            if let Ok(c) = std::str::from_utf8(&tmp) {
                return c.chars().next();
            }
        }
    }

    pub fn flush(&mut self) {
        self.stdout.flush().expect("stdout flush failed");
    }

    pub fn write(&mut self, bytes: &[u8]) {
        self.stdout.write_all(bytes).expect("stdout write failed");
    }

    pub fn writeln(&mut self, bytes: &[u8]) {
        self.write(bytes);
        self.write(b"\n");
    }

    pub fn get_input(&mut self) -> Option<String> {
        let mut input = String::new();

        self.write(PROMPT);
        self.flush();

        while let Some(c) = self.read_char() {
            match c {
                'q' => break,
                // ctrl-c
                '\u{3}' => break,
                // newline
                '\r' => {
                    self.write(b"\n");
                    if let Some(';') = input.trim().chars().last() {
                        self.history.push_back(input.clone());
                        if self.history.len() > HISTORY_SIZE {
                            self.history.pop_front();
                        }
                        return Some(input);
                    }
                    input.push('\n');
                    self.write(PROMPT);
                }
                // backspace
                '\u{7f}' => {
                    if !input.is_empty() {
                        self.write(b"\x08 \x08");
                        input.pop();
                    }
                }
                // start of arrow key sequence
                '\u{1b}' => {
                    if let Some('[') = self.read_char() {
                        match self.read_char() {
                            Some('A') => {
                                print!("up");
                            }
                            Some('B') => {
                                print!("down");
                            }
                            Some('D') => {
                                print!("left");
                            }
                            Some('C') => {
                                print!("right");
                            }
                            _ => (),
                        }
                    }
                }
                c => {
                    self.write(c.to_string().as_bytes());
                    input.push(c);
                }
            }
            self.flush();
        }
        None
    }
}
