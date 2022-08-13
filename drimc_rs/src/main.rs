//! Drim compiler CLI.

use std::{error::Error, fmt::Display, fs::File, io::Write, process::exit, str::FromStr};

use clap::Parser;
use drimc_rs::{
    ast2ir::ast2ir,
    backends,
    parser::{parser, ParserError, ParserMeta},
    typeck::typeck,
};

/// Optimization levels.
#[derive(Debug)]
enum Optimization {
    Numeric(usize),
    Size,
    Debugging,
    Speed,
}

impl Display for Optimization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Optimization::Numeric(n) => format!("{}", n),
                Optimization::Size => "z".to_string(),
                Optimization::Debugging => "g".to_string(),
                Optimization::Speed => "fast".to_string(),
            }
        )
    }
}

impl FromStr for Optimization {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Optimization::*;

        let levels = [
            (
                "0",
                "Disable all optimizations, for best results when debugging.",
                Numeric(0),
            ),
            (
                "1",
                "Enable basic optimizations, without significantly affecting build times.",
                Numeric(1),
            ),
            (
                "2",
                "Enable standard optimizations for release builds.",
                Numeric(2),
            ),
            (
                "3",
                "Optimize for speed at the expense of size.",
                Numeric(3),
            ),
            (
                "z",
                "Aggresively optimize for size rather than speed.",
                Size,
            ),
            (
                "g",
                "Enable some optimizations that do not interfere with debugging.",
                Debugging,
            ),
            (
                "fast",
                "Optimize for speed at the expense of strict standards compliance.",
                Speed,
            ),
        ];

        if s == "list" {
            eprintln!(
                "{}",
                levels
                    .iter()
                    .map(|(name, description, _)| format!("\n    -O{name} : {description}"))
                    .fold("optimization levels:".to_string(), |s, line| s + &line)
            );
            exit(1)
        } else {
            levels
                .into_iter()
                .find(|(name, _, _)| name == &s)
                .map(|(_, _, opt)| opt)
                .ok_or_else(|| "use -Olist to list".to_string())
        }
    }
}

/// Targets for compiling to.
#[derive(Debug)]
enum Target {
    // -------------------- C generation --------------------

    // Highest priority codegen, since it allows us to compile to the vast majority of possible
    // targets.
    /// Directly generate C source code.
    CSource,

    /// Pass generated C code into the system C compiler, and emit an assembly code file.
    Assembly,

    /// Pass Assembly code into the system assembler, and emit an object file.
    ObjectFile,

    /// Link generated object files using the system linker, and generate an executable file.
    Executable,

    /// Link generated object files using the system linker, and generate a shared object file.
    SharedObject,

    // -------------------- GPU generation --------------------

    // Medium-priority codegen, since GPUs typically can't be portably programmed in C and GPU
    // programming is a minor goal of the language.
    /// Directly generate SPIR-V code.
    Spirv,

    // TODO: add more GPU targets derived from spirv.

    // -------------------- WebAssembly generation --------------------

    // Very low-priority codegen, since efficient WebAssembly can be generated from C.
    /// Pass generated C code into the WebAssembly compiler, and generate WebAssembly text format
    /// code. At some point this might get replaced with direct WAT generation.
    Wat,

    /// Pass generated WAT code into a WebAssembly compiler, and generate WebAssembly binary format
    /// code.
    Wasm,

    // -------------------- Other language generation --------------------
    /// Directly generate Lua code.
    // Medium-priority codegen, since Lua is the only type of code that runs in e.g. plugins and
    // scripts in some software, and it is difficult to generate efficient Lua without direct
    // support from the compiler.
    Lua,

    /// Directly generate Python code.
    // Low-priority codegen; the same situation as Lua, but situations that require Python code with
    // no alternatives are much less common than situations that require Lua code with no
    // alternatives.
    Python,

    /// Directly generate Go code.
    // Extremely low-priority codegen; almost no valid use cases.
    Go,

    /// Directly generate Ada code.
    // Currently zero-priority codegen; no valid use cases whatsoever as far as I (Alex) can
    // determine.
    Ada,
}

impl Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Target::CSource => "c",
                Target::Assembly => "asm",
                Target::ObjectFile => "obj",
                Target::Executable => "exe",
                Target::SharedObject => "so",
                Target::Spirv => "spirv",
                Target::Wat => "wat",
                Target::Wasm => "wasm",
                Target::Lua => "lua",
                Target::Python => "py",
                Target::Go => "go",
                Target::Ada => "ada",
            }
        )
    }
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Target::*;

        let targets = [
            ("c", "C source code.", CSource),
            ("asm", "Assembly code.", Assembly),
            ("obj", "Object file.", ObjectFile),
            ("exe", "Executable binary file.", Executable),
            ("so", "Shared object file.", SharedObject),
            ("spirv", "Spir-V source code.", Spirv),
            ("wat", "WebAssembly text format code.", Wat),
            ("wasm", "WebAssembly binary format file.", Wasm),
            ("lua", "Lua source code.", Lua),
            ("python", "Python source code.", Python),
            ("go", "Go source code.", Go),
            ("ada", "Ada source code.", Ada),
        ];

        if s == "list" {
            eprintln!(
                "{}",
                targets
                    .iter()
                    .map(|(name, description, _)| format!("\n    -t{name} : {description}"))
                    .fold("targets:".to_string(), |s, line| s + &line)
            );
            exit(1)
        } else {
            targets
                .into_iter()
                .find(|(name, _, _)| name == &s)
                .map(|(_, _, target)| target)
                .ok_or_else(|| "use -tlist to list".to_string())
        }
    }
}

/// The Drim compiler.
#[derive(Parser, Debug)]
#[clap(version = "0.1.0")]
struct Args {
    /// Output file to generate.
    #[clap(short = 'o')]
    output: Option<String>,

    /// Optimization level. Use `-Olist` to list possible optimization levels.
    #[clap(short = 'O', default_value_t = Optimization::Numeric(0))]
    optimization: Optimization,

    /// Generate debug information in resultant code, if possible.
    #[clap(short = 'g')]
    debug_syms: bool,

    /// Target to generate code for. Use `-tlist` to list possible targets.
    #[clap(short = 't', default_value_t = Target::Executable)]
    target: Target,

    /// The source file to compile.
    source_file: String,
}

fn main() {
    fn main_e() -> Result<(), Box<dyn Error>> {
        let args = Args::parse();

        let source = std::fs::read_to_string(&args.source_file)?;
        let meta = ParserMeta::default();
        let ast = chumsky::Parser::parse(&parser(&meta), source).map_err(ParserError)?;
        let ast = typeck(ast)?;

        let ir = ast2ir(ast);

        match args.target {
            Target::CSource => {
                let c = backends::c::generate_c(&ir);

                let mut out_file = File::create("out.c")?;
                write!(out_file, "{}", c)?;
            }
            Target::Assembly => todo!(),
            Target::ObjectFile => todo!(),
            Target::Executable => todo!(),
            Target::SharedObject => todo!(),
            Target::Spirv => todo!(),
            Target::Wat => todo!(),
            Target::Wasm => todo!(),
            Target::Lua => todo!(),
            Target::Python => todo!(),
            Target::Go => todo!(),
            Target::Ada => todo!(),
        }

        Ok(())
    }

    match main_e() {
        Ok(()) => (),
        Err(e) => {
            eprintln!("drimc-rs fatal error: {}", e);
            exit(1);
        }
    }
}
