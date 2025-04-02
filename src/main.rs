#![feature(linked_list_cursors)]
#![feature(iterator_try_collect)]

use std::{collections::{linked_list::CursorMut, BTreeMap, LinkedList}, env, fmt::{self, Display}, fs, io, str::FromStr};

use derive_more::{Display, Error};
use once_cell::sync::Lazy;
use ctreg::regex;
use ordered_hash_map::OrderedHashSet;

type State = String;

#[derive(Debug, Display, Error)]
struct RuleParseErr;

#[derive(Debug, Display, Error)]
struct TapeParseErr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Symbol {
    ZERO,
    ONE,
    BLANK
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", char::from(self))
    }
}

impl FromStr for Symbol {
    type Err = RuleParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "0" => Symbol::ZERO,
            "1" => Symbol::ONE,
            "b" => Symbol::BLANK,
            _ => Err(RuleParseErr)?
        })
    }
}

impl From<&Symbol> for char {
    fn from(value: &Symbol) -> Self {
        match value {
            Symbol::ZERO => '0',
            Symbol::ONE => '1',
            Symbol::BLANK => 'b',
        }
    }
}

impl TryFrom<char> for Symbol {
    type Error = TapeParseErr;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            '0' => Symbol::ZERO,
            '1' => Symbol::ONE,
            'b' => Symbol::BLANK,
            _ => Err(TapeParseErr)?
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Direction {
    RIGHT,
    LEFT
}

impl Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Direction::RIGHT => 'R',
            Direction::LEFT => 'L',
        })
    }
}

impl FromStr for Direction {
    type Err = RuleParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "R" => Direction::RIGHT,
            "L" => Direction::LEFT,
            _ => Err(RuleParseErr)?
        })
    }
}

#[derive(Debug)]
struct Rule {
    i: State,
    j: Symbol,
    k: Symbol,
    s: State,
    d: Direction
}

impl Rule {
    pub fn split(self) -> (RuleCondition, RuleExecution) {
        (
            RuleCondition {
                i: self.i,
                j: self.j
            },
            RuleExecution {
                k: self.k,
                s: self.s,
                d: self.d
            }
        )
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {}, {}, {}, {})", self.i, self.j, self.k, self.s, self.d)
    }
}

regex!{pub RulePattern = r"\((?<i>[\w\d]+), ?(?<j>0|1|b), ?(?<k>0|1|b), ?(?<s>[\w\d]+), (?<d>R|L)\)"}

impl FromStr for Rule {
    type Err = RuleParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static RULE_PATTERN: Lazy<RulePattern> = Lazy::new(|| RulePattern::new());
        let captures = RULE_PATTERN.captures(s).ok_or(RuleParseErr)?;
        Ok(Rule {
            i: captures.i.content.to_owned(),
            j: captures.j.content.parse().expect("Already matched Regex."),
            k: captures.k.content.parse().expect("Already matched Regex."),
            s: captures.s.content.to_owned(),
            d: captures.d.content.parse().expect("Already matched Regex.")
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct RuleCondition {
    i: State,
    j: Symbol
}

#[derive(Debug)]
struct RuleExecution {
    k: Symbol,
    s: State,
    d: Direction
}

#[derive(Debug)]
struct TapeCursor<'a> (CursorMut<'a, Symbol>);

impl<'a> TapeCursor<'a> {
    pub fn move_dir(&mut self, dir: Direction) {
        match dir {
            Direction::RIGHT => {
                if self.0.peek_next().is_none() {
                    self.0.push_back(Symbol::BLANK);
                }
                self.0.move_next();
            },
            Direction::LEFT => {
                if self.0.peek_prev().is_none() {
                    self.0.push_front(Symbol::BLANK);
                }
                self.0.move_prev();
            },
        }
    }

    pub fn get_symbol(&mut self) -> &Symbol {
        self.0.current().expect("Can't reach an empty index.")
    }

    pub fn set_symbol(&mut self, sym: Symbol) {
        *self.0.current().expect("Can't reach an empty index.") = sym;
    }

    pub fn index(&self) -> usize {
        self.0.index().expect("Can't reach an empty index.")
    }
}

impl Display for TapeCursor<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.as_list().iter().map(|s| char::from(s)).collect::<String>())
    }
}

#[derive(Debug)]
struct Tape(LinkedList<Symbol>);

impl Tape {
    pub fn cursor_front_mut(&mut self) -> TapeCursor {
        TapeCursor(self.0.cursor_front_mut())
    }
}

impl Display for Tape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter().map(|s| char::from(s)).collect::<String>())
    }
}

impl FromStr for Tape {
    type Err = TapeParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Tape(s.chars().map(|c| c.try_into()).try_collect()?))
    }
}

fn main() {
    let mut args = env::args();
    args.next(); // skip the first arg which is the current executable

    let source = fs::read_to_string(args.next().unwrap_or("./main.tur".into())).expect("Missing source file.");
    let rules = source.split('\n').filter(|l| !l.is_empty()).map(|l| l.parse())
        .try_collect::<Vec<Rule>>().expect("Source parse fail.");

    let states: OrderedHashSet<State> = rules.iter().map(|r| r.i.clone()).collect();
    let mut state: State = states.iter().next().expect("Empty ruleset.").clone();
    let longest_state = states.iter()
        .reduce(|h, s| if s.len() > h.len() {s} else {h})
        .map(|s| s.len())
        .unwrap_or_default();

    let rule_map: BTreeMap<RuleCondition, RuleExecution> = rules.into_iter().map(|r| r.split()).collect();
    
    let mut input = String::new();
    println!("Please enter the input state of the tape:");
    io::stdin().read_line(&mut input).unwrap();
    input.pop(); // newline
    let mut tape: Tape = input.parse().expect("Tape parse fail.");

    let mut cursor = tape.cursor_front_mut();

    println!("Execution:\n");

    loop {
        println!("{}{} | {}", " ".repeat(longest_state - state.len()), state, cursor);
        println!("{} | {}^", " ".repeat(longest_state), " ".repeat(cursor.index()));
        if let Some(rule) = rule_map.get(&RuleCondition { i: state, j: *cursor.get_symbol() }) {
            cursor.set_symbol(rule.k);
            state = rule.s.clone();
            cursor.move_dir(rule.d);
        } else {
            break;
        }
    }
}
