DFF file utilities.

A DFF (DFF File Format) is a high-resolution audio file which
contains uncompressed DSD audio data along with information about
how the audio data is encoded. It can also optionally include an
[`ID3v2`](http://id3.org/) tag which contains metadata about the
music e.g. artist, album, etc.

# Examples

This example displays the metadata for the DFF file
`my/music.dff`.

```Rust
use dsf::DffFile;
use std::path::Path;

let path = Path::new("my/music.dff");

match DffFile::open(path) {
    Ok(dff_file) => {
        println!("DFF file metadata:\n\n{}", dff_file);
    }
    Err(error) => {
        println!("Error: {}", error);
    }
}
```