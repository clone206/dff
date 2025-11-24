DFF file utilities.

A DFF (DFF File Format) is a high-resolution audio file which
contains DSD audio data along with information about
how the audio data is encoded. It can also optionally include an
[`ID3v2`](http://id3.org/) tag which contains metadata about the
music e.g. artist, album, etc.

This library allows you to read DFF file metadata, and provides a reference to the underlying DFF file itself. It is up to the user to decide how to read the sound data, using metadata including offset and audio length from the DffFile object to seek to and read the audio bytes from the underlying file.

Only supports ID3 tags that appear at the end of the file, not those found in the property chunk. DST is not supported. Mostly geared toward mono and stereo audio.

# Examples

This example displays the metadata for the DFF file
`my/music.dff`.

```rust
use dff_meta::DffFile;
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