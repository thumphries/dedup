#**dedup**lication tool

Command line tool to list or delete redundant files in a directory tree.

    Usage: dedup PATH [-r|--recursive] [-d|--delete] [-l|--longer-names]
                      [-k|--list-keepers]

> **WARNING**: This was an afternoon hack. Not exhaustively tested, and
> doesn't handle all IO errors gracefully. In particular, use the `-d` flag
> with care.

`dedup` identifies duplicates by first comparing file sizes, then
SHA256 hashes. For each group of identical file, it selects the
shorter filename (by default) as 'keeper', then prints the others to
stdout as deletion candidates.

Alternatively, supply the `-d` flag and all deletion candidates will
be deleted. Use this with care - don't get hit by a <a
href="#race">race condition</a>. You may prefer to first salvage the
'keep' candidates - these can be listed with `-k`.

Pass `-l` or `--longer-names` to keep longer filenames. By default,
only the duplicate with the shortest name will be kept.

##Examples

- List deletion candidates for a folder: `dedup /path/to/purge`
- Actually delete all such candidates from a folder: `dedup -d /path/to/purge`
- ... delete recursively: `dedup -r -d /path/to/purge`
- List the files to be kept for a folder: `dedup -k /path/to/purge`

<h2 name="race">Race condition</h2>

If a file on the 'keep' list is deleted / modified during `dedup -d`
operation, that data (and all redundant copies) will be totally lost.
For this reason, it is best to backup the keepers with something like
`dedup -k /path/to/purge | xargs -I{} cp "{}" /backup/path/` first.
