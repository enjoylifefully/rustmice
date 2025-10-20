use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

struct Grid {
    cells: Vec<Vec<char>>,
    start_pos: (usize, usize),
}


fn format_prolog_input(grid: &Grid) -> String {
    let mut prolog_facts = String::new();

    for (r, row) in grid.cells.iter().enumerate() {
        let cells_str: Vec<String> = row.iter().map(|c| c.to_string()).collect();
        let prolog_list = cells_str.join(", ");

        prolog_facts.push_str(&format!("mapa({}, [{}]).\n", r, prolog_list));
    }

    let (r, c) = grid.start_pos;
    prolog_facts.push_str(&format!("estado_inicial(pos({}, {})).\n", r, c));

    prolog_facts
}

//pode tirar
fn print_grid_to_console(grid: &Grid) {
    println!("--- Grid Estática Definida ---");
    for row in &grid.cells {
        let row_str: String = row.iter().collect();
        println!("{}", row_str);
    }
    println!("------------------------------");
}

fn main() {
    let cells = vec![
        vec!['c', 'c', 'c', 'c', 'c'],
        vec!['c', 'p', 'c', 'p', 'p'],
        vec!['j', 'c', 'c', 'c', 'o'], // 'j' em (2,0), 'o' em (2,4)
    ];
    let start_pos = (2, 0);

    let grid = Grid { cells, start_pos };

    let prolog_input = format_prolog_input(&grid);
    print_grid_to_console(&grid);

    let prolog_command = "swipl";

    let mut child = Command::new(prolog_command)
        .args(["-q", "-s", "solver.pl"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Falha ao iniciar o SWI-Prolog. Ele está instalado e no PATH?");

    let mut child_stdin = child.stdin.take().expect("Falha ao pegar stdin do processo");

    std::thread::spawn(move || {
        child_stdin
            .write_all(prolog_input.as_bytes())
            .expect("Falha ao escrever no stdin do Prolog");
    });

    let stdout = child.stdout.take().expect("Falha ao pegar stdout");
    let reader = BufReader::new(stdout);

    println!("Prolog está pensando...");

    match reader.lines().next() {
        Some(Ok(line)) => {
            println!("\n=== Solução Encontrada pelo Prolog ===");
            println!("{}", line);
        }
        Some(Err(e)) => eprintln!("Erro ao ler saída do Prolog: {}", e),
        None => eprintln!("Prolog não produziu nenhuma saída."),
    }

    let output = child.wait_with_output().expect("Falha ao esperar pelo processo Prolog");
    if !output.status.success() {
        eprintln!(
            "\n--- Erro do Prolog (stderr) ---:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}
