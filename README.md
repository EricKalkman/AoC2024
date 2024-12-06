# aoc2024

[Advent of Code](https://adventofcode.com) 2024


## Usage

`lein run` to run all days using the Clojure code, printing results and timing information.

Inputs are expected in the `inputs/` directory as *.inp files.

To run scheme code with Chez Scheme, `cd` to the `chez` directory and run `run.sh`

Note that although all of the Scheme solutions are written in portable R6RS Scheme, the functions
used to get timing info are specific to Chez Scheme. If you wish to run the code using another
Scheme interpreter, you may comment out the import of `chezscheme` in `chez/util.scm`, and then
modify the `time-exec` macro defined therein; all other code should be compliant.

## License

Copyright &copy; 2024 Eric Kalkman

This project is licensed under the [GNU General Public License v3.0][license].

[license]: https://choosealicense.com/licenses/gpl-3.0
