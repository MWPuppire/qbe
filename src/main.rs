use qbe::*;

fn main() {
    let ctx = QbeContext::new();
    let a_opts = QbeDecl::default().with_export_as("a");
    let global_a = ctx.global_ext(0, &a_opts).unwrap();
    let test_opts = QbeDecl::default().with_export_as("test");
    let test = ctx
        .function_ext(&[], &test_opts, |f| {
            let start = f.start();
            let end_block = f.forward_declare_block();
            let x5 = f.initialize(QbeType::Double);
            let i2 = f.initialize(QbeType::Word);
            let x1 = f.copy(0.1)?;
            let x2 = f.add(0.2, x1)?;
            let x3 = f.sub(x2, 0.3)?;
            let loop_block = f.block()?;
            let x4 = f.phi(start, x3, loop_block, x5)?;
            let i1 = f.phi(start, 0, loop_block, i2)?;
            f.reassign(x5, |f| f.add(x4, x4))?;
            f.reassign(i2, |f| f.add(i1, 1))?;
            let c0 = f.le(x5, 1.0f64)?;
            f.jnz(c0, loop_block, &end_block)?;
            f.block_at(end_block)?;
            f.store(i2, global_a)?;
            Ok(())
        })
        .unwrap();
    let printf = ctx.extern_func::<QbeValue>("printf", QbeType::Word);
    let format_str = ctx.global("%d\n").unwrap();
    let main_opts = QbeDecl::default().with_export_as("main");
    ctx.function_ext(&[], &main_opts, |f| {
        f.call(test, [] as [QbeValue; 0])?;
        let a = f.load(global_a, QbeType::Word)?;
        f.call_va(printf, [format_str], [a])?;
        let eq = f.eq(a, 55)?;
        let out = f.sub(1, eq)?;
        Ok(out)
    })
    .unwrap();
    #[cfg(feature = "qbe-compile")]
    println!("{}", ctx.to_assembly().unwrap());
    #[cfg(not(feature = "qbe-compile"))]
    {
        eprintln!("Cannot create assembly with this build; emitting QBE IR instead");
        println!("{}", ctx.to_ir());
    }
}
