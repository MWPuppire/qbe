use qbe::*;

fn main() {
    let mut ctx = QbeContext::new();
    let a_opts = QbeDeclBuilder::default().export_as("a").build().unwrap();
    ctx.global_ext(0, &a_opts).unwrap();
    let test_opts = QbeDeclBuilder::default().export_as("test").build().unwrap();
    let test = ctx.function_ext(&[], &test_opts, |f| {
        let start = f.start();
        let end_block = f.forward_declare_block();
        let global_a = f.global_symbol("a");
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
        let c0 = f.le(x5, 4607182418800017408i64)?;
        f.jnz(c0, loop_block, &end_block)?;
        f.block_at(end_block)?;
        f.store(i2, global_a)?;
        Ok(())
    }).unwrap();
    /*
    let main_opts = QbeDeclBuilder::default().export_as("main").build().unwrap();
    ctx.function_ext(&[], &main_opts, |f| {
        f.call(test, [0i32])?;
        let global_a = f.global_symbol("a");
        let eq = f.eq(global_a, 55)?;
        let out = f.sub(1, eq)?;
        Ok(out)
    }).unwrap();
    */
    println!("{}", ctx.to_assembly().unwrap());
}
