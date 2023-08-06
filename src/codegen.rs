//! Functions to convert a tree with intrinsics to assembly and run it.

use crate::analysis::{Effect, Intrinsic, Type};
use crate::Tree;

#[derive(Debug)]
struct Assembly {
    main: String,
    rest: String,
}

pub fn compile_program_x64(tree: &mut Tree) {
    use std::io::Write;

    let name = match tree.token.source.name.rfind('.') {
        Some(i) => tree.token.source.name[..i].to_string(),
        None => "a.out".to_string(),
    };

    let mut assembler = std::process::Command::new("gcc")
        .arg("-x")
        .arg("assembler-with-cpp")
        .arg("-nostdlib")
        .arg("-Wall")
        .arg("-g")
        .arg("-o")
        .arg(name.clone())
        .arg("-")
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    let stdin = assembler.stdin.as_mut().unwrap();

    let assembly = tree.get_mut::<Assembly>().unwrap();

    let prologue = "#include <sys/syscall.h>\n\n.global _start\n_start:\n";
    assembly.main.insert_str(0, prologue);
    let epilogue = "movq $SYS_exit, %rax\nmovq $0, %rdi\nsyscall\n\n";
    assembly.main.push_str(epilogue);

    let mut write_lines = |s: &str| {
        for line in s.lines() {
            if !line.is_empty() {
                let chars: Vec<char> = line.chars().collect();
                if !(chars[0] == '#' || chars[chars.len() - 1] == ':') {
                    write!(stdin, "\t").unwrap();
                }
            }
            writeln!(stdin, "{}", line).unwrap();
        }
    };

    write_lines(&assembly.main);
    write_lines(&assembly.rest);

    if !assembler.wait().unwrap().success() {
        panic!("assembler failed")
    }

    let output = std::process::Command::new(format!("./{}", name))
        .output()
        .unwrap();
    println!(
        "status: {}\nstderr: {}\nstdout: {}",
        output.status,
        std::str::from_utf8(&output.stderr).unwrap(),
        std::str::from_utf8(&output.stdout).unwrap(),
    );
}

pub fn compile_intrinsics_x64(tree: &mut Tree) {
    fn compile_intrinsics_x64(tree: &mut Tree, labels: &mut usize) {
        use std::fmt::Write;

        let mut main = String::new();
        let mut rest = String::new();
        for child in &mut tree.children {
            main.push_str(&match child.get::<Intrinsic>() {
                Some(Intrinsic::Push(int)) => format!("movq ${}, %rax\npushq %rax\n", int),
                Some(Intrinsic::Pop) => "popq %rax\n".to_string(),
                Some(Intrinsic::Read) => {
                    "movq (%rsp), %rax\nmovq (%rax), %rax\nmovq %rax, (%rsp)\n".to_string()
                }
                Some(Intrinsic::Write) => "popq %rax\npopq %rcx\nmovq %rcx, (%rax)\n".to_string(),
                Some(Intrinsic::Sp) => "pushq %rsp\n".to_string(),
                Some(Intrinsic::Add) => "popq %rax\naddq %rax, (%rsp)\n".to_string(),
                Some(Intrinsic::Label(s)) => format!("{}:\n", s),
                Some(Intrinsic::Jumpz(s)) => {
                    format!("popq %rax\ntestq %rax, %rax\njz {}\n", s)
                }
                Some(Intrinsic::Call) => "popq %rax\ncallq *%rax\n".to_string(),
                Some(Intrinsic::Func) => {
                    compile_intrinsics_x64(child, labels);
                    let assembly = child.get::<Assembly>().unwrap();
                    let effect = match &child.get::<Effect>().unwrap().outputs[0] {
                        Type::Fun(e) => e,
                        _ => unreachable!(),
                    };
                    let inputs = effect.inputs.iter().fold(0, |a, t| a + t.size());
                    let outputs = effect.outputs.iter().fold(0, |a, t| a + t.size());

                    let mut prologue = String::new();
                    for _ in 0..(inputs / 8) {
                        write!(prologue, "movq {}(%rsp), %rax\npushq %rax\n", inputs).unwrap();
                    }

                    let mut epilogue = String::new();
                    writeln!(epilogue, "movq {}(%rsp), %rcx", outputs).unwrap();
                    for i in 0..(outputs / 8) {
                        let offset = outputs - 8 - i * 8;
                        write!(
                            epilogue,
                            "movq {}(%rsp), %rax\nmovq %rax, {}(%rsp)\n",
                            offset,
                            offset + inputs + 8
                        )
                        .unwrap();
                    }
                    writeln!(epilogue, "addq ${}, %rsp", inputs + 8).unwrap();
                    write!(epilogue, "pushq %rcx\nretq\n").unwrap();

                    *labels += 1;
                    let label = format!("_${}", *labels - 1);

                    write!(
                        rest,
                        "{}:\n{}{}{}\n\n{}",
                        label, prologue, assembly.main, epilogue, assembly.rest
                    )
                    .unwrap();
                    format!("leaq {}(%rip), %rax\npushq %rax\n", label)
                }
                None => return,
            });
        }
        tree.insert(Assembly { main, rest });
    }
    compile_intrinsics_x64(tree, &mut 0)
}
