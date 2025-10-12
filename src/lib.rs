//! DFF file utilities.
//!
//! A DFF (DFF File Format) is a high-resolution audio file which
//! contains uncompressed DSD audio data along with information about
//! how the audio data is encoded. It can also optionally include an
//! [`ID3v2`](http://id3.org/) tag which contains metadata about the
//! music e.g. artist, album, etc.
//!
//! # Examples
//!
//! This example displays the metadata for the DFF file
//! `my/music.dff`.
//!
//!```no_run
//! use dsf::DffFile;
//! use std::path::Path;
//!
//! let path = Path::new("my/music.dff");
//!
//! match DffFile::open(path) {
//!     Ok(dff_file) => {
//!         println!("DFF file metadata:\n\n{}", dff_file);
//!     }
//!     Err(error) => {
//!         println!("Error: {}", error);
//!     }
//! }
//! ```

mod id3_display;
mod model;

use crate::model::*;
use id3::Tag;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::SeekFrom;
use std::io::prelude::*;
use std::path::Path;
use std::u64;

pub struct DffFile {
    file: File,
    frm_chunk: FormDsdChunk,
    dsd_data_offset: u64,
    dsd_audio_size: u64,
}

// Helper: scan forward for a chunk header matching `want_label`, error if `abort_label` appears first.
// Returns the 12-byte header (ID + size). Skips payload (and pad byte if size is odd) of non-matching chunks.
fn scan_until(
    file: &mut File,
    want_label: u32,
    abort_label: Option<u32>,
) -> Result<[u8; CHUNK_HEADER_SIZE as usize], Error> {
    loop {
        let mut hdr = [0u8; CHUNK_HEADER_SIZE as usize];
        match file.read_exact(&mut hdr) {
            Ok(_) => {}
            Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
                return Err(Error::Eof);
            }
            Err(e) => return Err(Error::IoError(e)),
        }
        let ck_id = u32_from_byte_buffer(&hdr, 0);
        let ck_data_size = u64_from_byte_buffer(&hdr, 4);
        if ck_id == want_label {
            return Ok(hdr);
        } else if Some(ck_id) == abort_label {
            return Err(Error::PrematureTagFound(
                String::from_utf8_lossy(&ck_id.to_be_bytes()).to_string(),
            ));
        } else {
            // Skip payload + pad if odd
            file.seek(SeekFrom::Current(ck_data_size as i64))?;
            if ck_data_size & 1 == 1 {
                file.seek(SeekFrom::Current(1))?;
            }
        }
    }
}

impl DffFile {
    /// Attempt to open and parse the metadata of DFF file in
    /// read-only mode. Sample data is not read into memory to keep
    /// the memory footprint small.
    ///
    /// # Errors
    ///
    /// This function will return an error if `path` does not exist or
    /// is not a readable and valid DFF file.
    ///
    pub fn open(path: &Path) -> Result<DffFile, Error> {
        let mut file = File::open(path)?;
        let mut chunk_buf16 = [0u8; 16];
        let mut prop_buf4 = [0u8; 4];

        // FORM (FRM8)
        file.read_exact(&mut chunk_buf16)?;
        let mut frm_chunk = FormDsdChunk::try_from(chunk_buf16)?;

        // FVER
        file.read_exact(&mut chunk_buf16)?;
        let fver_chunk = FormatVersionChunk::try_from(chunk_buf16)?;
        frm_chunk
            .chunk
            .local_chunks
            .insert(FVER_LABEL, LocalChunk::FormatVersion(fver_chunk));

        // Locate PROP (abort if DSD encountered first)
        // Locate PROP (abort if DSD encountered first)
                let mut hdr_buf = scan_until(&mut file, PROP_LABEL, Some(DSD_LABEL))?;
                chunk_buf16[0..12].copy_from_slice(&hdr_buf);
        // Read property_type (4 bytes) then build 16-byte buffer
        file.read_exact(&mut prop_buf4)?;
        chunk_buf16[12..16].copy_from_slice(&prop_buf4);

        let pc = PropertyChunk::try_from(chunk_buf16)?;
        frm_chunk
            .chunk
            .local_chunks
            .insert(PROP_LABEL, LocalChunk::Property(pc));

        let prop_data_size = match frm_chunk.chunk.local_chunks.get(&PROP_LABEL) {
            Some(LocalChunk::Property(prop)) => prop.chunk.header.ck_data_size,
            _ => return Err(Error::PropChunkHeader),
        };
        let prop_data_offset = file.stream_position()? - 4;

        if let Some(LocalChunk::Property(prop_chunk_inner)) =
            frm_chunk.chunk.local_chunks.get_mut(&PROP_LABEL)
        {
            while file.stream_position()? < prop_data_offset + prop_data_size as u64
                && file.read_exact(&mut hdr_buf).is_ok()
            {
                let ck_id = u32_from_byte_buffer(&hdr_buf, 0);
                let ck_data_size = u64_from_byte_buffer(&hdr_buf, 4);

                match ck_id {
                    FS_LABEL => {
                        let mut chunk_data_buffer: [u8; 4] = [0; 4];
                        file.read_exact(&mut chunk_data_buffer)?;
                        let fs_chunk = SampleRateChunk::try_from({
                            let mut buf = [0u8; 16];
                            buf[0..12].copy_from_slice(&hdr_buf);
                            buf[12..16].copy_from_slice(&chunk_data_buffer);
                            buf
                        })?;
                        prop_chunk_inner
                            .chunk
                            .local_chunks
                            .insert(FS_LABEL, LocalChunk::SampleRate(fs_chunk));
                    }
                    CHNL_LABEL => {
                        let mut data_buf = vec![0u8; ck_data_size as usize];
                        file.read_exact(&mut data_buf)?;
                        let mut full_buf = Vec::with_capacity(12 + data_buf.len());
                        full_buf.extend_from_slice(&hdr_buf);
                        full_buf.extend_from_slice(&data_buf);
                        if let Ok(chnl_chunk) = ChannelsChunk::try_from(full_buf.as_slice()) {
                            prop_chunk_inner
                                .chunk
                                .local_chunks
                                .insert(CHNL_LABEL, LocalChunk::Channels(chnl_chunk));
                        }
                    }
                    COMP_LABEL => {
                        let mut data_buf = vec![0u8; ck_data_size as usize];
                        file.read_exact(&mut data_buf)?;
                        let mut full_buf = Vec::with_capacity(12 + data_buf.len());
                        full_buf.extend_from_slice(&hdr_buf);
                        full_buf.extend_from_slice(&data_buf);
                        if let Ok(cmpr_chunk) = CompressionTypeChunk::try_from(full_buf.as_slice())
                        {
                            prop_chunk_inner
                                .chunk
                                .local_chunks
                                .insert(COMP_LABEL, LocalChunk::CompressionType(cmpr_chunk));
                        }
                    }
                    ABS_TIME_LABEL => {
                        let mut data_buf = vec![0u8; ck_data_size as usize];
                        file.read_exact(&mut data_buf)?;
                        let mut full_buf = Vec::with_capacity(12 + data_buf.len());
                        full_buf.extend_from_slice(&hdr_buf);
                        full_buf.extend_from_slice(&data_buf);
                        if let Ok(abs_chunk) = AbsoluteStartTimeChunk::try_from(full_buf.as_slice())
                        {
                            prop_chunk_inner
                                .chunk
                                .local_chunks
                                .insert(ABS_TIME_LABEL, LocalChunk::AbsoluteStartTime(abs_chunk));
                        }
                    }
                    LS_CONF_LABEL => {
                        let mut data_buf = vec![0u8; ck_data_size as usize];
                        file.read_exact(&mut data_buf)?;
                        let mut full_buf = Vec::with_capacity(12 + data_buf.len());
                        full_buf.extend_from_slice(&hdr_buf);
                        full_buf.extend_from_slice(&data_buf);
                        if let Ok(lsco_chunk) =
                            LoudspeakerConfigChunk::try_from(full_buf.as_slice())
                        {
                            prop_chunk_inner
                                .chunk
                                .local_chunks
                                .insert(LS_CONF_LABEL, LocalChunk::LoudspeakerConfig(lsco_chunk));
                        }
                    }
                    _ => {
                        file.seek(SeekFrom::Current(ck_data_size as i64))?;
                    }
                }
            }
        }

        hdr_buf = scan_until(&mut file, DSD_LABEL, None)?;
        let dsd_data_offset = file.stream_position()?;
        let dsd_chunk = DsdChunk::try_from(hdr_buf)?;
        let dsd_audio_size = dsd_chunk.chunk.header.ck_data_size;
        frm_chunk
            .chunk
            .local_chunks
            .insert(DSD_LABEL, LocalChunk::Dsd(dsd_chunk));

        // Seek past raw DSD audio data
        file.seek(SeekFrom::Current(dsd_audio_size as i64))?;

        // Scan any remaining chunks inside the FORM chunk boundary for ID3
        let form_end = frm_chunk.chunk.header.ck_data_size + CHUNK_HEADER_SIZE; // total bytes from start (FORM header at offset 0)
        loop {
            let pos = file.stream_position()?;
            // Ensure we have room for another chunk header fully inside FORM
            if pos + CHUNK_HEADER_SIZE > form_end {
                break;
            }

            // Read potential header
            if let Err(e) = file.read_exact(&mut hdr_buf) {
                if e.kind() == io::ErrorKind::UnexpectedEof {
                    break;
                }
                return Err(e.into());
            }

            let ck_id = u32_from_byte_buffer(&hdr_buf, 0);
            let ck_data_size = u64_from_byte_buffer(&hdr_buf, std::mem::size_of::<ID>());
            // If payload would exceed FORM boundary, stop.
            if pos + CHUNK_HEADER_SIZE + ck_data_size > form_end {
                break;
            }

            if ck_id != ID3_LABEL {
                // Skip unknown/non-ID3 chunk payload
                file.seek(SeekFrom::Current(ck_data_size as i64))?;
                continue;
            }

            // Read ID3 data
            let mut data = vec![0u8; ck_data_size as usize];
            file.read_exact(&mut data)?;

            // Use non-deprecated API: read_from2 requires Read + Seek; use a Cursor over the data
            let mut cursor = std::io::Cursor::new(&data);
            let tag = id3::Tag::read_from2(&mut cursor).ok();
            let id3_chunk = Id3Chunk {
                chunk: Chunk::new(ChunkHeader {
                    ck_id,
                    ck_data_size,
                }),
                tag,
            };

            frm_chunk
                .chunk
                .local_chunks
                .insert(ID3_LABEL, LocalChunk::Id3(id3_chunk));

            // Only one expected; break after storing
            break;
        }

        Ok(DffFile {
            file,
            frm_chunk,
            dsd_data_offset,
            dsd_audio_size,
        })
    }

    /// Return a reference to the underlying [File](std::fs::File).
    #[must_use]
    pub fn file(&self) -> &File {
        &self.file
    }

    pub fn get_dsd_data_offset(&self) -> u64 {
        self.dsd_data_offset
    }

    pub fn get_audio_length(&self) -> u64 {
        self.dsd_audio_size
    }

    pub fn get_num_channels(&self) -> Result<usize, Error> {
        let prop_chunk = match self.frm_chunk.chunk.local_chunks.get(&PROP_LABEL) {
            Some(LocalChunk::Property(prop)) => prop,
            _ => return Err(Error::PropChunkHeader),
        };
        match prop_chunk.chunk.local_chunks.get(&CHNL_LABEL) {
            Some(LocalChunk::Channels(chnl)) => Ok(chnl.num_channels as usize),
            _ => return Err(Error::ChnlChunkHeader),
        }
    }

    pub fn get_sample_rate(&self) -> Result<u32, Error> {
        let prop_chunk = match self.frm_chunk.chunk.local_chunks.get(&PROP_LABEL) {
            Some(LocalChunk::Property(prop)) => prop,
            _ => return Err(Error::PropChunkHeader),
        };
        match prop_chunk.chunk.local_chunks.get(&FS_LABEL) {
            Some(LocalChunk::SampleRate(fs)) => Ok(fs.sample_rate),
            _ => return Err(Error::FsChunkHeader),
        }
    }

    pub fn get_form_chunk_size(&self) -> u64 {
        self.frm_chunk.chunk.header.ck_data_size + CHUNK_HEADER_SIZE
    }

    pub fn get_file_size(&self) -> Result<u64, io::Error> {
        let metadata = self.file.metadata()?;
        Ok(metadata.len())
    }

    pub fn id3_tag(&self) -> &Option<Tag> {
        match self.frm_chunk.chunk.local_chunks.get(&ID3_LABEL) {
            Some(LocalChunk::Id3(id3_chunk)) => &id3_chunk.tag,
            _ => &None,
        }
    }

    pub fn get_dff_version(&self) -> Result<u32, Error> {
        let fver_chunk = match self.frm_chunk.chunk.local_chunks.get(&FVER_LABEL) {
            Some(LocalChunk::FormatVersion(fver)) => fver,
            _ => return Err(Error::FverChunkHeader),
        };
        Ok(fver_chunk.format_version)
    }

    /// Recursively dump all parsed chunks to stderr with helpful details.
    /// Does not re-read the file; uses the already parsed model.
    pub fn dump_chunks_stderr(&self) {
        // Root FORM/FRM8
        eprintln!(
            "FRM8 form='{}' size={} (includes header)",
            fourcc(self.frm_chunk.form_type),
            self.get_form_chunk_size()
        );
        // Walk children of FORM
        walk_local_chunks(&self.frm_chunk.chunk.local_chunks, 1);
        // Helpful footer for audio payload
        eprintln!(
            "DSD payload: offset={} bytes, length={} bytes",
            self.dsd_data_offset, self.dsd_audio_size
        );
    }
}
impl fmt::Display for DffFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "File size: {} bytes\nForm Chunk Size: {} bytes\nDSD Audio Offset: {} bytes\nAudio Length: {} bytes\nChannels: {}\nSample Rate: {} Hz\nDFF Version: {}\nID3 Tag:\n{}",
            self.get_file_size().unwrap_or(0),
            self.get_form_chunk_size(),
            self.get_dsd_data_offset(),
            self.get_audio_length(),
            self.get_num_channels().unwrap_or(0),
            self.get_sample_rate().unwrap_or(0),
            self.get_dff_version().unwrap_or(0),
            if let Some(tag) = &self.id3_tag() {
                id3_display::id3_tag_to_string(tag)
            } else {
                String::from("No ID3 tag present.")
            }
        )
    }
}

// ----- Introspection helpers -----

fn indent(depth: usize) -> String {
    const PAD: &str = "  ";
    let mut s = String::new();
    for _ in 0..depth {
        s.push_str(PAD);
    }
    s
}

fn fourcc(id: u32) -> String {
    let b = id.to_be_bytes();
    String::from_utf8_lossy(&b).to_string()
}

fn walk_local_chunks(map: &HashMap<ID, LocalChunk>, depth: usize) {
    // Stable order by ID for nicer output
    let mut items: Vec<(&ID, &LocalChunk)> = map.iter().collect();
    items.sort_by_key(|(id, _)| **id);

    for (_id, lc) in items {
        match lc {
            LocalChunk::FormatVersion(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}FVER ({}) size={} version={}",
                    i,
                    fourcc(ck_id),
                    size,
                    c.format_version
                );
            }
            LocalChunk::Property(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}PROP ({}) size={} type='{}' children={}",
                    i,
                    fourcc(ck_id),
                    size,
                    fourcc(c.property_type),
                    c.chunk.local_chunks.len()
                );
                // Recurse into PROP children
                walk_local_chunks(&c.chunk.local_chunks, depth + 1);
            }
            LocalChunk::SampleRate(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}FS   ({}) size={} sample_rate={} Hz",
                    i,
                    fourcc(ck_id),
                    size,
                    c.sample_rate
                );
            }
            LocalChunk::Channels(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                let ids: Vec<String> = c
                    .ch_id
                    .iter()
                    .map(|v| fourcc(*v))
                    .collect();
                eprintln!(
                    "{}CHNL ({}) size={} channels={} ids=[{}]",
                    i,
                    fourcc(ck_id),
                    size,
                    c.num_channels,
                    ids.join(", ")
                );
            }
            LocalChunk::CompressionType(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}CMPR ({}) size={} type='{}' name=\"{}\"",
                    i,
                    fourcc(ck_id),
                    size,
                    fourcc(c.compression_type),
                    c.compression_name
                );
            }
            LocalChunk::AbsoluteStartTime(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}ABSS ({}) size={} {:02}:{:02}:{:02}.samples={}",
                    i,
                    fourcc(ck_id),
                    size,
                    c.hours,
                    c.minutes,
                    c.seconds,
                    c.samples
                );
            }
            LocalChunk::LoudspeakerConfig(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}LSCO ({}) size={} config=0x{:04X}",
                    i,
                    fourcc(ck_id),
                    size,
                    c.ls_config
                );
            }
            LocalChunk::Dsd(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                eprintln!(
                    "{}DSD  ({}) size={} (raw DSD audio)",
                    i,
                    fourcc(ck_id),
                    size
                );
            }
            LocalChunk::Id3(c) => {
                let i = indent(depth);
                let ck_id = c.chunk.header.ck_id;
                let size = c.chunk.header.ck_data_size;
                if let Some(tag) = &c.tag {
                    let frames = tag.frames().count();
                    eprintln!(
                        "{}ID3  ({}) size={} frames={}",
                        i,
                        fourcc(ck_id),
                        size,
                        frames
                    );
                } else {
                    eprintln!(
                        "{}ID3  ({}) size={} (no tag parsed)",
                        i,
                        fourcc(ck_id),
                        size
                    );
                }
            }
        }
    }
}

/// Return a `u64` which starts from `index` in the specified byte
/// buffer, interpretting the bytes as little-endian.
fn u64_from_byte_buffer(buffer: &[u8], index: usize) -> u64 {
    let mut byte_array: [u8; 8] = [0; 8];
    byte_array.copy_from_slice(&buffer[index..index + 8]);

    u64::from_be_bytes(byte_array)
}

/// Return a `u32` which starts from `index` in the specified byte
/// buffer, interpretting the bytes as little-endian.
fn u32_from_byte_buffer(buffer: &[u8], index: usize) -> u32 {
    let mut byte_array: [u8; 4] = [0; 4];
    byte_array.copy_from_slice(&buffer[index..index + 4]);

    u32::from_be_bytes(byte_array)
}

impl Chunk {
    pub fn new(header: ChunkHeader) -> Chunk {
        Chunk {
            header,
            local_chunks: HashMap::new(),
        }
    }
}

impl FormDsdChunk {
    #[inline]
    pub fn is_valid(&self) -> bool {
        self.chunk.header.ck_id == u32::from_be_bytes(*b"FRM8") && self.form_type == DSD_LABEL
    }
}

// IMPLEMENTATION: Convert 16‑byte header into FormDsdChunk
impl TryFrom<[u8; 16]> for FormDsdChunk {
    type Error = Error;

    fn try_from(buf: [u8; 16]) -> Result<Self, Self::Error> {
        // Big‑endian helpers
        let be_u32 = |i: usize| {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[i..i + 4]);
            u32::from_be_bytes(a)
        };
        let be_u64 = |i: usize| {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[i..i + 8]);
            u64::from_be_bytes(a)
        };

        let ck_id = be_u32(0);
        let ck_data_size = be_u64(4);
        let form_type = be_u32(12);
        let header = ChunkHeader {
            ck_id,
            ck_data_size,
        };
        let chunk = FormDsdChunk {
            chunk: Chunk::new(header),
            form_type,
        };

        if !chunk.is_valid() {
            return Err(Error::FormChunkHeader);
        }
        if chunk.form_type != DSD_LABEL {
            return Err(Error::FormTypeMismatch);
        }

        Ok(chunk)
    }
}

impl TryFrom<[u8; 16]> for FormatVersionChunk {
    type Error = Error;

    fn try_from(buf: [u8; 16]) -> Result<Self, Self::Error> {
        let be_u32 = |i: usize| {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[i..i + 4]);
            u32::from_be_bytes(a)
        };
        let be_u64 = |i: usize| {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[i..i + 8]);
            u64::from_be_bytes(a)
        };

        let ck_id = be_u32(0);
        let ck_data_size = be_u64(4);
        let version = be_u32(12);
        let header = ChunkHeader {
            ck_id,
            ck_data_size,
        };
        let chunk = FormatVersionChunk {
            chunk: Chunk::new(header),
            format_version: version,
        };

        if chunk.chunk.header.ck_id != FVER_LABEL {
            return Err(Error::FverChunkHeader);
        }
        if chunk.chunk.header.ck_data_size != 4 {
            return Err(Error::FverChunkSize);
        }

        Ok(chunk)
    }
}

impl TryFrom<[u8; 16]> for PropertyChunk {
    type Error = Error;
    fn try_from(buf: [u8; 16]) -> Result<Self, Self::Error> {
        let be_u32 = |i: usize| {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[i..i + 4]);
            u32::from_be_bytes(a)
        };
        let be_u64 = |i: usize| {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[i..i + 8]);
            u64::from_be_bytes(a)
        };

        let ck_id = be_u32(0);
        let ck_data_size = be_u64(4);
        // Must at least contain 4 bytes for property_type.
        if ck_data_size < 4 {
            return Err(Error::ChnlChunkSize); // reuse generic size error not ideal; kept minimal
        }
        let property_type = be_u32(12);

        let header = ChunkHeader {
            ck_id,
            ck_data_size,
        };
        let chunk = PropertyChunk {
            chunk: Chunk::new(header),
            property_type,
        };

        if chunk.chunk.header.ck_id != PROP_LABEL {
            return Err(Error::PropChunkHeader);
        }
        if chunk.property_type != SND_LABEL {
            return Err(Error::PropChunkType);
        }
        Ok(chunk)
    }
}

impl TryFrom<[u8; 16]> for SampleRateChunk {
    type Error = Error;
    fn try_from(buf: [u8; 16]) -> Result<Self, Self::Error> {
        let be_u32 = |i: usize| {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[i..i + 4]);
            u32::from_be_bytes(a)
        };
        let be_u64 = |i: usize| {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[i..i + 8]);
            u64::from_be_bytes(a)
        };

        let ck_id = be_u32(0);
        let ck_data_size = be_u64(4);
        let sample_rate = be_u32(12);

        let header = ChunkHeader {
            ck_id,
            ck_data_size,
        };
        let chunk = SampleRateChunk {
            chunk: Chunk::new(header),
            sample_rate,
        };

        if chunk.chunk.header.ck_id != FS_LABEL {
            return Err(Error::FsChunkHeader);
        }
        if chunk.chunk.header.ck_data_size != 4 {
            return Err(Error::FsChunkSize);
        }
        Ok(chunk)
    }
}

impl TryFrom<&[u8]> for ChannelsChunk {
    type Error = Error;
    fn try_from(buf: &[u8]) -> Result<Self, Self::Error> {
        if buf.len() < 14 {
            return Err(Error::ChnlChunkSize);
        }

        let ck_id = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[0..4]);
            u32::from_be_bytes(a)
        };

        let ck_data_size = {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[4..12]);
            u64::from_be_bytes(a)
        };

        // Total expected length = 12(header) + ck_data_size
        if buf.len() as u64 != 12 + ck_data_size {
            return Err(Error::ChnlChunkSize);
        }

        let num_channels = {
            let mut a = [0u8; 2];
            a.copy_from_slice(&buf[12..14]);
            u16::from_be_bytes(a)
        };

        if num_channels != 1 && num_channels != 2 {
            return Err(Error::ChnlNumber);
        }

        // Each channel id is 4 bytes
        let expected_ids_bytes = (num_channels as usize) * 4;
        if 14 + expected_ids_bytes != buf.len() {
            return Err(Error::ChnlChunkSize);
        }

        let mut channel_ids = Vec::with_capacity(num_channels as usize);
        let mut idx = 14;
        for _ in 0..num_channels {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[idx..idx + 4]);
            channel_ids.push(u32::from_be_bytes(a));
            idx += 4;
        }

        let header = ChunkHeader {
            ck_id,
            ck_data_size,
        };

        let chunk = ChannelsChunk {
            chunk: Chunk::new(header),
            num_channels,
            ch_id: channel_ids,
        };

        if chunk.chunk.header.ck_id != CHNL_LABEL {
            return Err(Error::ChnlChunkHeader);
        }
        if expected_ids_bytes != chunk.ch_id.len() * 4 {
            return Err(Error::ChnlChunkSize);
        }
        Ok(chunk)
    }
}

impl TryFrom<&[u8]> for CompressionTypeChunk {
    type Error = Error;
    fn try_from(buf: &[u8]) -> Result<Self, Self::Error> {
        // Need at least 12 (header) + 4 (compression type)
        if buf.len() < 16 {
            return Err(Error::CmprChunkSize);
        }

        // Header
        let ck_id = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[0..4]);
            u32::from_be_bytes(a)
        };

        let ck_data_size = {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[4..12]);
            u64::from_be_bytes(a)
        };

        if buf.len() as u64 != 12 + ck_data_size || ck_data_size < 4 {
            return Err(Error::CmprChunkSize);
        }

        let compression_type = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[12..16]);
            u32::from_be_bytes(a)
        };

        // Remaining bytes (if any) are a UTF-8 / ASCII name, often null terminated
        let name_bytes = if ck_data_size > 4 {
            &buf[16..(12 + ck_data_size as usize)]
        } else {
            &[]
        };

        let compression_name = match std::str::from_utf8(name_bytes) {
            Ok(inner_str) => inner_str.to_string(),
            Err(_) => String::new(),
        };

        let chunk = CompressionTypeChunk {
            chunk: Chunk::new(ChunkHeader {
                ck_id,
                ck_data_size,
            }),
            compression_type,
            compression_name,
        };

        if chunk.chunk.header.ck_id != COMP_LABEL {
            return Err(Error::CmprChunkHeader);
        }
        // New check: must be 'DSD '
        // DST not yet implemented
        if chunk.compression_type != DSD_LABEL || chunk.compression_name == "DST Encoded" {
            return Err(Error::CmprTypeMismatch);
        }
        Ok(chunk)
    }
}

impl TryFrom<&[u8]> for AbsoluteStartTimeChunk {
    type Error = Error;
    fn try_from(buf: &[u8]) -> Result<Self, Self::Error> {
        // Need full header (12) + payload (8)
        if buf.len() < 20 {
            return Err(Error::AbssChunkSize);
        }
        // Chunk ID
        let ck_id = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[0..4]);
            u32::from_be_bytes(a)
        };
        // Data size (must be 8 bytes for: hours(2) + minutes(1) + seconds(1) + samples(4))
        let ck_data_size = {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[4..12]);
            u64::from_be_bytes(a)
        };
        if ck_data_size != 8 || buf.len() as u64 != 12 + ck_data_size {
            return Err(Error::AbssChunkSize);
        }

        // Payload layout (big-endian):
        // bytes 12..14 : U16 hours
        // byte  14     : U8 minutes
        // byte  15     : U8 seconds
        // bytes 16..20 : U32 samples (sample offset within that second)
        let hours = {
            let mut a = [0u8; 2];
            a.copy_from_slice(&buf[12..14]);
            u16::from_be_bytes(a)
        };
        let minutes = buf[14];
        let seconds = buf[15];
        let samples = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[16..20]);
            u32::from_be_bytes(a)
        };

        let chunk = AbsoluteStartTimeChunk {
            chunk: Chunk::new(ChunkHeader {
                ck_id,
                ck_data_size,
            }),
            hours,
            minutes,
            seconds,
            samples,
        };

        if chunk.chunk.header.ck_id != ABS_TIME_LABEL {
            return Err(Error::AbssChunkHeader);
        }
        Ok(chunk)
    }
}

impl TryFrom<&[u8]> for LoudspeakerConfigChunk {
    type Error = Error;
    fn try_from(buf: &[u8]) -> Result<Self, Self::Error> {
        // Need header (12) + 2 bytes payload
        if buf.len() < 14 {
            return Err(Error::LscoChunkSize);
        }
        let ck_id = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[0..4]);
            u32::from_be_bytes(a)
        };
        if ck_id != LS_CONF_LABEL {
            return Err(Error::LscoChunkHeader);
        }
        let ck_data_size = {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[4..12]);
            u64::from_be_bytes(a)
        };

        if buf.len() as u64 != 12 + ck_data_size {
            return Err(Error::LscoChunkSize);
        }

        let ls_config = {
            let mut a = [0u8; 2];
            a.copy_from_slice(&buf[12..14]);
            u16::from_be_bytes(a)
        };

        let chunk = LoudspeakerConfigChunk {
            chunk: Chunk::new(ChunkHeader {
                ck_id,
                ck_data_size,
            }),
            ls_config,
        };

        // LSCO payload is exactly 2 bytes (U16 loudspeaker configuration code)
        if chunk.chunk.header.ck_data_size != 2 {
            return Err(Error::LscoChunkSize);
        }
        Ok(chunk)
    }
}

impl fmt::Display for DsdChunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Copy packed field to a local to avoid unaligned reference.
        let size = self.chunk.header.ck_data_size;
        write!(f, "Audio Length = {} bytes", size)
    }
}

impl TryFrom<[u8; 12]> for DsdChunk {
    type Error = Error;

    fn try_from(buf: [u8; 12]) -> Result<Self, Self::Error> {
        let ck_id = {
            let mut a = [0u8; 4];
            a.copy_from_slice(&buf[0..4]);
            u32::from_be_bytes(a)
        };
        if ck_id != DSD_LABEL {
            return Err(Error::DsdChunkHeader);
        }

        let ck_data_size = {
            let mut a = [0u8; 8];
            a.copy_from_slice(&buf[4..12]);
            u64::from_be_bytes(a)
        };
        if ck_data_size == 0 {
            return Err(Error::DsdChunkSize);
        }
        Ok(DsdChunk {
            chunk: Chunk::new(ChunkHeader {
                ck_id,
                ck_data_size,
            }),
        })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::BlockSizePerChannelNonStandard => write!(
                f,
                "A fmt chunk is expected to specify its block size per channel as {}.",
                BLOCK_SIZE
            ),
            Error::ChannelNum => {
                f.write_str("A fmt chunk’s channel num is expected to be in the range 1–6.")
            }
            Error::ChannelType => {
                f.write_str("A fmt chunk’s channel type is expected to be in the range 1–7.")
            }
            Error::DataChunkHeader => f.write_str("A data chunk must start with the bytes 'data'."),
            Error::DsdChunkHeader => f.write_str("A DSD chunk must start with the bytes 'DSD '."),
            Error::DsdChunkSize => f.write_str("A DSD chunk must not have size 0."),
            Error::FmtChunkHeader => f.write_str("A fmt chunk must start with the bytes 'fmt '."),
            Error::FmtChunkSize => {
                f.write_str("A fmt chunk is expected to specify its size as 52 bytes.")
            }
            Error::FormatId => f.write_str("A fmt chumk must specifiy a format ID of 0."),
            Error::FormatVersion => f.write_str("A fmt chunk must specify version 1."),
            Error::Id3Error(id3_error) => write!(f, "Id3 error: {}", id3_error),
            Error::IoError(io_error) => write!(f, "IO error: {}", io_error),
            Error::PrematureTagFound(e) => {
                write!(f, "Chunk {} was found before it should have been.", e)
            }
            Error::ReservedNotZero => {
                f.write_str("A fmt chunk’s reserved space is expected to be zero filled.")
            }
            Error::ChannelIndexOutOfRange => f.write_str("Channel index is out of range."),
            Error::SampleIndexOutOfRange => f.write_str("Sample index is out of range."),
            Error::FrameIndexOutOfRange => f.write_str("Frame index is out of range."),
            Error::FormChunkHeader => f.write_str("FORM chunk must start with 'FRM8'."),
            Error::FormTypeMismatch => f.write_str("FORM chunk form type must be 'DSD '."),
            Error::FverChunkHeader => f.write_str("Format Version chunk must start with 'FVER'."),
            Error::FverChunkSize => f.write_str("FVER chunk data size must be 4."),
            Error::FverUnsupportedVersion => {
                f.write_str("Unsupported format version in FVER chunk.")
            }
            Error::PropChunkHeader => f.write_str("Property chunk must start with 'PROP'."),
            Error::PropChunkType => f.write_str("Property chunk type must be 'SND '."),
            Error::FsChunkHeader => f.write_str("Sample rate chunk must start with 'FS  '."),
            Error::FsChunkSize => f.write_str("FS chunk size must be 4."),
            Error::ChnlChunkHeader => f.write_str("Channels chunk must start with 'CHNL'."),
            Error::ChnlChunkSize => f.write_str("CHNL chunk size does not match channel data."),
            Error::ChnlNumber => f.write_str("CHNL number not supported."),
            Error::CmprChunkHeader => f.write_str("Compression type chunk must start with 'CMPR'."),
            Error::CmprChunkSize => f.write_str("CMPR chunk size invalid or inconsistent."),
            Error::AbssChunkHeader => {
                f.write_str("Absolute start time chunk must start with 'ABSS'.")
            }
            Error::AbssChunkSize => f.write_str("ABSS chunk size invalid."),
            Error::LscoChunkHeader => {
                f.write_str("Loudspeaker config chunk must start with 'LSCO'.")
            }
            Error::LscoChunkSize => f.write_str("LSCO chunk size invalid or inconsistent."),
            Error::CmprTypeMismatch => f.write_str("Compression type must be 'DSD '."),
            Error::Eof => f.write_str("Unexpected end of file."),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Id3Error(id3_error) => Some(id3_error),
            Error::IoError(io_error) => Some(io_error),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::IoError(error)
    }
}

impl From<id3::Error> for Error {
    fn from(error: id3::Error) -> Self {
        Error::Id3Error(error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_file() {
        let filename = "test.dff";
        let path = Path::new(filename);

        match DffFile::open(path) {
            Ok(dff_file) => {
                println!("DFF file metadata:\n\n{}", dff_file);
            }
            Err(error) => {
                println!("Error: {}", error);
            }
        }
    }
}
