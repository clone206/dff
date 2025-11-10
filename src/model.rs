//! DFF File Model

use std::io;
use std::collections::HashMap;

use id3::Tag;

use crate::DffFile;

pub type Uchar = u8;
pub type Ushort = u16;
pub type Ulong = u32;
pub type DoubleUlong = u64;
pub type ID = u32;

pub const CHUNK_HEADER_SIZE: DoubleUlong = 12;
pub const DSD_LABEL: ID = u32::from_be_bytes(*b"DSD ");
pub const FVER_LABEL: ID = u32::from_be_bytes(*b"FVER");
pub const PROP_LABEL: ID = u32::from_be_bytes(*b"PROP");
pub const SND_LABEL: ID = u32::from_be_bytes(*b"SND ");
pub const FS_LABEL: ID = u32::from_be_bytes(*b"FS  ");
pub const CHNL_LABEL: ID = u32::from_be_bytes(*b"CHNL");
pub const COMP_LABEL: ID = u32::from_be_bytes(*b"CMPR");
pub const ABS_TIME_LABEL: ID = u32::from_be_bytes(*b"ABSS");
pub const LS_CONF_LABEL: ID = u32::from_be_bytes(*b"LSCO");
pub const ID3_LABEL: ID = u32::from_be_bytes(*b"ID3 ");

#[derive(Debug)]
pub enum Error {
    DsdChunkHeader,
    DsdChunkSize,
    Id3Error(id3::Error, DffFile),
    IoError(io::Error),
    FormChunkHeader,
    FormTypeMismatch,
    FverChunkHeader,
    FverChunkSize,
    FverUnsupportedVersion,
    PropChunkHeader,
    PropChunkType,
    FsChunkHeader,
    FsChunkSize,
    ChnlChunkHeader,
    ChnlChunkSize,
    ChnlNumber,
    CmprChunkHeader,
    CmprChunkSize,
    AbssChunkHeader,
    AbssChunkSize,
    LscoChunkHeader,
    LscoChunkSize,
    CmprTypeMismatch,
    PrematureTagFound(String),
    Eof,
}

#[derive(Debug, Clone)]
pub enum LocalChunk {
    FormatVersion(FormatVersionChunk),
    Property(PropertyChunk),
    SampleRate(SampleRateChunk),
    Channels(ChannelsChunk),
    CompressionType(#[allow(dead_code)]CompressionTypeChunk),
    AbsoluteStartTime(#[allow(dead_code)]AbsoluteStartTimeChunk),
    LoudspeakerConfig(#[allow(dead_code)]LoudspeakerConfigChunk),
    Dsd(#[allow(dead_code)] DsdChunk),
    Id3(Id3Chunk), // NEW
}

#[repr(C, packed)]
#[derive(Debug, Copy, Clone)]
pub struct ChunkHeader {
    pub ck_id: ID,
    pub ck_data_size: DoubleUlong,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub header: ChunkHeader,
    pub local_chunks: HashMap<ID, LocalChunk>,
}

#[derive(Debug, Clone)]
pub struct FormDsdChunk {
    pub chunk: Chunk,
    pub form_type: ID,
}

#[derive(Debug, Clone)]
pub struct FormatVersionChunk {
    pub chunk: Chunk,
    pub format_version: Ulong,
}

#[derive(Debug, Clone)]
pub struct PropertyChunk {
    pub chunk: Chunk,
    pub property_type: ID,
}

#[derive(Debug, Clone)]
pub struct SampleRateChunk {
    pub chunk: Chunk,
    pub sample_rate: Ulong,
}

#[derive(Debug, Clone)]
pub struct ChannelsChunk {
    pub chunk: Chunk,
    pub num_channels: Ushort,
    pub ch_id: Vec<ID>, // length = channel_count
}

#[derive(Debug, Clone)]
pub struct CompressionTypeChunk {
    pub chunk: Chunk,
    pub compression_type: ID,
    pub compression_name: String,
}

#[derive(Debug, Clone)]
pub struct AbsoluteStartTimeChunk {
    pub chunk: Chunk,
    #[allow(dead_code)]
    pub hours: Ushort,
    #[allow(dead_code)]
    pub minutes: Uchar,
    #[allow(dead_code)]
    pub seconds: Uchar,
    #[allow(dead_code)]
    pub samples: Ulong,
}

#[derive(Debug, Clone)]
pub struct LoudspeakerConfigChunk {
    pub chunk: Chunk,
    #[allow(dead_code)]
    pub ls_config: Ushort,
}

#[derive(Debug, Clone)]
pub struct DsdChunk {
    pub chunk: Chunk,
}

#[derive(Debug, Clone)]
pub struct Id3Chunk {
    #[allow(dead_code)]
    pub chunk: Chunk,
    pub tag: Option<Tag>,
}