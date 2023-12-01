use std::fs;

fn get_line_size(content: &str) -> usize {
    let mut size = 0;
    for c in content.chars() {
        if c == '\n' {
            return size;
        }
        size += 1;
    }
    return 0;
}

fn get_nb_line(content: &str) -> usize {
    let mut size = 0;
    for c in content.chars() {
        if c == '\n' {
            size += 1;
        }
    }
    return size;
}

fn is_visible(x: usize, y: usize, idx: usize, content: &str) -> bool {
    let line_size = get_line_size(&content);
    let nb_line = get_nb_line(&content);
    let c = content.as_bytes()[idx];

    let mut visible_left = true;
    let mut visible_top = true;
    let mut visible_right = true;
    let mut visible_down = true;

    for i in 0..x {
        let new_idx = y * (line_size + 1) + i;
        let new_c = content.as_bytes()[new_idx];
        if new_c >= c {
            visible_left = false;
        }
        //println!("{new_idx} {new_c}");
    }
    for i in 0..y {
        let new_idx = i * (line_size + 1) + x;
        let new_c = content.as_bytes()[new_idx];
        if new_c >= c {
            visible_top = false;
        }
        //println!("{new_idx} {new_c}");
    }
    for i in x+1..line_size {
        let new_idx = y * (line_size + 1) + i;
        let new_c = content.as_bytes()[new_idx];
        if new_c >= c {
            visible_right = false;
        }
        //println!("{new_idx} {new_c}");
    }
    for i in y+1..nb_line {
        let new_idx = i * (line_size + 1) + x;
        let new_c = content.as_bytes()[new_idx];
        if new_c >= c {
            visible_down = false;
        }
        //println!("{new_idx} {new_c}");
    }
    return visible_left || visible_top || visible_right || visible_down;
}

fn star1(line_size: usize, nb_line: usize, content: &str) {
    let mut nb_visible = line_size * 2 + (nb_line - 2) * 2;
    //println!("{line_size} {nb_line}");

    for i in 1..nb_line - 1 {
        for j in 1..line_size - 1 {
            let idx = i * (line_size + 1) + j;
            //let c = content.as_bytes()[idx];
            //println!("tree: {idx} {c}");
            let visible = is_visible(j, i, idx, &content);
            if visible {
                nb_visible += 1
            }
            //println!("{visible}");
        }
        //println!("");
    }
    println!("number of tree visible: {nb_visible}");
}

fn compute_size(x:usize, y:usize, content: &str) -> usize{
    let line_size = get_line_size(&content);
    let nb_line = get_nb_line(&content);

    let mut visible_left = 1;
    let mut visible_top = 1;
    let mut visible_right = 1;
    let mut visible_down = 1;
    let idx = y * (line_size + 1) + x;
    let mut current_height = content.as_bytes()[idx];

    for i in (1..x).rev() {
        let new_idx = y * (line_size + 1) + i;
        let new_c = content.as_bytes()[new_idx];
        if new_c < current_height {
            visible_left += 1;
            current_height = new_c;
        } else if new_c >= current_height {
            break;
        }
        //println!("{new_idx} {new_c} {visible_left}");
    }
    //println!("{visible_left}");
    current_height = content.as_bytes()[idx];
    for i in (1..y).rev() {
        let new_idx = i * (line_size + 1) + x;
        let new_c = content.as_bytes()[new_idx];
        if new_c < current_height {
            visible_top += 1;
            current_height = new_c;
        } else if new_c >= current_height {
            break;
        }
        // println!("{new_idx} {new_c} {visible_top}");
    }
    // println!("visible top: {visible_top}");
    //println!("y = {y}, nb_line = {nb_line}");
    current_height = content.as_bytes()[idx];
    for i in y+1..nb_line-1 {
        let new_idx = i * (line_size + 1) + x;
        let new_c = content.as_bytes()[new_idx];
        if new_c < current_height {
            visible_down += 1;
            current_height = new_c;
        } else if new_c >= current_height {
            break;
        }
        // println!("{new_idx} {new_c} {visible_down}");
    }
    // println!("visible down: {visible_down}");
    current_height = content.as_bytes()[idx];
    for i in x+1..line_size-1 {
        let new_idx = y * (line_size + 1) + i;
        let new_c = content.as_bytes()[new_idx];
        if new_c < current_height {
            visible_right += 1;
            current_height = new_c;
        } else if new_c >= current_height {
            break;
        }
        // println!("{new_idx} {new_c} {visible_right}");
    }
    // println!("visible right: {visible_right}");
    return visible_top * visible_left * visible_down * visible_right;
}

fn star2(line_size: usize, nb_line: usize, content: &str) {
    let mut highest_score = 0;
    for y in 1..line_size - 1 {
        for x in 1..nb_line - 1 {
            // let idx = y * (line_size + 1) + x;
            // let c = content.as_bytes()[idx];
            // println!("tree: {idx} {c}");
            let tree_score = compute_size(x, y, content);
            if tree_score > highest_score {
                highest_score = tree_score;
            }
        }
    }
    println!("highest tree score: {highest_score}");
}

fn main() {
    let content = fs::read_to_string("input")
        .expect("error reading");

    let line_size = get_line_size(&content);
    let nb_line = get_nb_line(&content);

    star1(line_size, nb_line, &content);
    star2(line_size, nb_line, &content);
}
