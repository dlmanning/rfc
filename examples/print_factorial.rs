use rpl::analysis::analyze;
use rpl::core::Interner;
use rpl::lower::lower;
use rpl::parse::parse;
use rpl::registry::{InterfaceRegistry, LowererRegistry};
use rpl::vm::disasm::disassemble;
use rpl::{read_leb128_u32, read_u16};

fn main() {
    let source = r#"
@ Factorial using recursion
@ Usage: 5 fact -> 120

<< -> n <<
    IF n 0 ==
    THEN
        1
    ELSE
        n 1 - fact n *
    END
>> >>
"fact" STO

5 fact
"#;

    let mut interner = Interner::new();
    let mut interfaces = InterfaceRegistry::new();
    let mut lowerers = LowererRegistry::new();

    rpl_stdlib::register_interfaces(&mut interfaces);
    rpl_stdlib::register_lowerers(&mut lowerers);

    let nodes = parse(source, &interfaces, &mut interner).expect("parse failed");
    let analysis = analyze(&nodes, &interfaces, &interner, &rpl::analysis::Context::empty());
    let program = lower(&nodes, &interfaces, &lowerers, &interner, &analysis).expect("lowering failed");

    println!("=== Outer Bytecode ({} bytes) ===\n", program.code.len());

    let instrs = disassemble(&program.code, 0, 1000);
    for instr in &instrs {
        println!("{:4}  {:<40}  {}", instr.pc, instr.text, instr.bytes);
    }

    println!("\n=== Outer Rodata ({} bytes) ===", program.rodata.len());
    if !program.rodata.is_empty() {
        println!("{:?}", String::from_utf8_lossy(&program.rodata));
    }

    // Extract and disassemble the inner program (MakeProgram at offset 0)
    // Format: MakeProgram <param_count:leb128> <rodata_len:leb128> <rodata> <code_len:leb128> <code> <spans>
    let mut offset = 1; // skip opcode
    let _param_count = read_leb128_u32(&program.code, &mut offset).unwrap();
    let rodata_len = read_leb128_u32(&program.code, &mut offset).unwrap() as usize;
    let inner_rodata = &program.code[offset..offset + rodata_len];
    offset += rodata_len;
    let code_len = read_leb128_u32(&program.code, &mut offset).unwrap() as usize;
    let inner_code = &program.code[offset..offset + code_len];
    offset += code_len;
    let span_count = read_u16(&program.code, &mut offset).unwrap();

    println!("\n=== Inner Program (factorial function) ===");
    println!("Code: {} bytes, Rodata: {} bytes, Spans: {}\n", code_len, rodata_len, span_count);

    println!("Inner Rodata: {:?}", String::from_utf8_lossy(inner_rodata));
    println!();

    let inner_instrs = disassemble(inner_code, 0, 1000);
    for instr in &inner_instrs {
        println!("{:4}  {:<40}  {}", instr.pc, instr.text, instr.bytes);
    }

    // Debug: show what analysis knows about types
    println!("\n=== Analysis: Node Stacks ({} entries) ===", analysis.node_stacks.len());
    let mut stacks: Vec<_> = analysis.node_stacks.iter().collect();
    stacks.sort_by_key(|(span, _)| span.start().offset());
    for (span, snapshot) in stacks {
        let src_slice = &source[span.start().offset() as usize..span.end().offset() as usize];
        println!("  {:3}-{:3} {:20?}: tos={:?}, nos={:?}",
            span.start().offset(), span.end().offset(),
            src_slice,
            snapshot.tos, snapshot.nos);
    }

    println!("\n=== Analysis: Definitions ===");
    for def in analysis.symbols.definitions() {
        println!("  {}: {:?} (kind={:?})",
            def.name, def.value_type, def.kind);
    }
}

