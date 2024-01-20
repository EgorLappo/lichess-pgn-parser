# lichess pgn parser

This program parses official [Lichess `.pgn` files](https://database.lichess.org) into tabular (`.csv`) data. The `.pgn`s are read from stdin and written to stdout to be piped, e.g. like 

```
zstdcat file.pgn.zst | ./lichess-pgn-parser > file.csv
```

There are two mutually incompatible options, `--keep` and `--cut`. Without options, the moves are given without move number indications, game clock, and evaluation for ease of downstream processing. With `--keep`, the original move string it kept as-is, which is faster but harder to analyze later. When `--cut n` is used, with `n` an integer, only first `n` moves are kept in the `.csv`, each in its own column (`move1`, `move2`, etc.). See help for other options, which are more obvious.

## Help message

```
lichess-pgn-parser. featuring: attoparsec

Usage: lichess-pgn-parser-exe [-k|--keep] [-n|--header] 
                              [-s|--sep ARG] 
                              [-c|--cut ARG]

  convert a lichess pgn file into tabular format, streaming

Available options:
  -h,--help                Show this help text
  -k,--keep                keep the move string as-is (e.g. with time,
                           evaluation data)
  -n,--header              print the csv header line
  -s,--sep ARG             separator character for output (default: tab)
  -c,--cut ARG             cut the move string after ARG moves, making
                           individual columns (default: 0)
```
