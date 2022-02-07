fn main() {
    let graph_content = "Node (Input first1,Input first1);Node (Input first2,Input first2);Node (Input second1,Input second1);Node (Input second2,Input second2);Node (Input carry_in,Input carry_in);Node (Entity 2,Variable res1);Node (Entity 3,Variable res2);Node (Entity 4,Variable outc);Node (Variable res1 0,Variable res1);Node (Variable tmpc 0,Variable tmpc);Node (Variable res2 1,Variable res2);Node (Variable outc 1,Variable outc);Node (Output out1,Output out1);Node (Output out2,Output out2);Node (Output carry_out,Output carry_out);Node (Entity 5,Operation xor);Node (Entity 6,Operation xor);Node (Entity 7,Operation and);Node (Entity 8,Operation and);Node (Entity 9,Operation or);Node (Variable first_result 5,Variable first_result);Node (Variable carry_res1 7,Variable carry_res1);Node (Variable carry_res2 8,Variable carry_res2);Node (Entity 10,Operation xor);Node (Entity 11,Operation xor);Node (Entity 12,Operation and);Node (Entity 13,Operation and);Node (Entity 14,Operation or);Node (Variable first_result 10,Variable first_result);Node (Variable carry_res1 12,Variable carry_res1);Node (Variable carry_res2 13,Variable carry_res2);Node (Splitter 15,Splitter 2);Node (Splitter 16,Splitter 2);Node (Splitter 17,Splitter 2);Node (Splitter 18,Splitter 2);Node (Splitter 19,Splitter 2);Node (Splitter 20,Splitter 2);Node (Splitter 21,Splitter 2);Node (Splitter 22,Splitter 2);Node (Splitter 23,Splitter 2);Node (Splitter 24,Splitter 2);Node (Splitter 25,Splitter 2);Node (Splitter 26,Splitter 2);NodeToOutput (Variable res1 0,0,Output out1);NodeToOutput (Variable res2 1,0,Output out2);NodeToOutput (Variable outc 1,0,Output carry_out);NodeToNode (Entity 5,0,Variable first_result 5,0);NodeToNode (Entity 7,0,Variable carry_res1 7,0);NodeToNode (Entity 8,0,Variable carry_res2 8,0);NodeToNode (Variable carry_res1 7,0,Entity 9,0);NodeToNode (Variable carry_res2 8,0,Entity 9,1);NodeToNode (Entity 10,0,Variable first_result 10,0);NodeToNode (Entity 12,0,Variable carry_res1 12,0);NodeToNode (Entity 13,0,Variable carry_res2 13,0);NodeToNode (Variable carry_res1 12,0,Entity 14,0);NodeToNode (Variable carry_res2 13,0,Entity 14,1);NodeToNode (Variable first_result 5,0,Splitter 15,0);NodeToNode (Splitter 15,0,Entity 6,0);NodeToNode (Splitter 15,1,Entity 8,0);InputToNode (Input first1,Splitter 16,0);NodeToNode (Splitter 16,0,Entity 5,0);NodeToNode (Splitter 16,1,Entity 7,0);InputToNode (Input second1,Splitter 17,0);NodeToNode (Splitter 17,0,Entity 5,1);NodeToNode (Splitter 17,1,Entity 7,1);InputToNode (Input carry_in,Splitter 18,0);NodeToNode (Splitter 18,0,Entity 6,1);NodeToNode (Splitter 18,1,Entity 8,1);NodeToNode (Entity 6,0,Splitter 19,0);NodeToNode (Splitter 19,0,Variable res1 0,0);NodeToNode (Splitter 19,1,Variable tmpc 0,0);NodeToNode (Entity 9,0,Splitter 20,0);NodeToNode (Splitter 20,0,Variable res1 0,0);NodeToNode (Splitter 20,1,Variable tmpc 0,0);NodeToNode (Variable first_result 10,0,Splitter 21,0);NodeToNode (Splitter 21,0,Entity 11,0);NodeToNode (Splitter 21,1,Entity 13,0);InputToNode (Input first2,Splitter 22,0);NodeToNode (Splitter 22,0,Entity 10,0);NodeToNode (Splitter 22,1,Entity 12,0);InputToNode (Input second2,Splitter 23,0);NodeToNode (Splitter 23,0,Entity 10,1);NodeToNode (Splitter 23,1,Entity 12,1);NodeToNode (Variable tmpc 0,0,Splitter 24,0);NodeToNode (Splitter 24,0,Entity 11,1);NodeToNode (Splitter 24,1,Entity 13,1);NodeToNode (Entity 11,0,Splitter 25,0);NodeToNode (Splitter 25,0,Variable res2 1,0);NodeToNode (Splitter 25,1,Variable outc 1,0);NodeToNode (Entity 14,0,Splitter 26,0);NodeToNode (Splitter 26,0,Variable res2 1,0);NodeToNode (Splitter 26,1,Variable outc 1,0);";

    let layout = mclc::generate_layout(graph_content);

    layout.generate_svg("./placement.svg");
    let placement = layout.placement();

    let cmds = placement.place_commands();
    std::fs::write(
        "./commands.txt",
        cmds.into_iter().fold("".to_string(), |mut acc, c| {
            acc.push_str(&c);
            acc.push('\n');
            acc
        }),
    )
    .unwrap();

    println!("Hello, world!");
}
