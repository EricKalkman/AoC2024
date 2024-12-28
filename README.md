# aoc2024

[Advent of Code](https://adventofcode.com) 2024


## Usage

Inputs are expected in the `inputs/` directory as *.inp files.

`lein run` to run all days using the Clojure code, printing results and timing information.

Running the Scheme code may be a bit more involved due to differences in distributions. To run it,
`cd` to the `chez` directory and run `run.sh`. The `SCHEME_EXEC` environmental variable
controls the command and associated options to run. It defaults to `scheme --libdirs . --script `,
appropriate for Chez Scheme on some systems.

Note that although all of the Scheme solutions are written in portable R6RS Scheme, the functions
used to get timing info are specific to Chez Scheme. If you wish to run the code using another
Scheme interpreter, you may comment out the import of `chezscheme` in `chez/util.scm`, and then
modify the `time-exec` macro defined therein. In addition, a `format` function must be available
to `runner.scm`; all other code should be R6RS-compliant.

To run the OCaml code, `cd` to the `ocaml` directory and run with `dune exec aoc2024`.

## License

Copyright &copy; 2024 Eric Kalkman

This project is licensed under the [GNU General Public License v3.0][license].

[license]: https://choosealicense.com/licenses/gpl-3.0
