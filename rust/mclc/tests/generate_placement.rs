#[test]
fn placement() {
    let content = "Node (Input first,Input first);Node (Input second,Input second);Node (Input carry_in,Input carry_in);Node (Entity 0,Operation xor);Node (Entity 1,Operation xor);Node (Entity 2,Operation and);Node (Entity 3,Operation and);Node (Entity 4,Operation or);Node (Variable first_result 0,Variable first_result);Node (Variable carry_res1 2,Variable carry_res1);Node (Variable carry_res2 3,Variable carry_res2);Node (Output first,Output first);Node (Output carry_out,Output carry_out);Node (Splitter 5,Splitter 2);Node (Splitter 6,Splitter 2);Node (Splitter 7,Splitter 2);Node (Splitter 8,Splitter 2);NodeToNode (Entity 0,0,Variable first_result 0,0);NodeToOutput (Entity 1,0,Output first);NodeToNode (Entity 2,0,Variable carry_res1 2,0);NodeToNode (Entity 3,0,Variable carry_res2 3,0);NodeToNode (Variable carry_res1 2,0,Entity 4,0);NodeToNode (Variable carry_res2 3,0,Entity 4,1);NodeToOutput (Entity 4,0,Output carry_out);InputToNode (Input first,Splitter 5,0);NodeToNode (Splitter 5,0,Entity 0,0);NodeToNode (Splitter 5,1,Entity 2,0);InputToNode (Input second,Splitter 6,0);NodeToNode (Splitter 6,0,Entity 0,1);NodeToNode (Splitter 6,1,Entity 2,1);NodeToNode (Variable first_result 0,0,Splitter 7,0);NodeToNode (Splitter 7,0,Entity 1,0);NodeToNode (Splitter 7,1,Entity 3,0);InputToNode (Input carry_in,Splitter 8,0);NodeToNode (Splitter 8,0,Entity 1,1);NodeToNode (Splitter 8,1,Entity 3,1);";

    let result = mclc::generate_layout(content);
    result.generate_svg("/Users/leon/Documents/coding/haskell/mcl/rust/mclc/image.svg");

    let block_placement = result.placement();
    let commands = block_placement.place_commands();

    std::fs::write(
        "/Users/leon/Documents/coding/haskell/mcl/rust/mclc/commands.txt",
        commands.into_iter().fold("".to_string(), |mut acc, cmd| {
            acc.push_str(&cmd);
            acc.push('\n');
            acc
        }),
    )
    .unwrap();

    todo!()
}
