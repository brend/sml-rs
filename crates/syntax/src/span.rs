#[cfg(feature = "spans")]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}
#[cfg(feature = "spans")]
impl Span {
    pub const fn new(lo: u32, hi: u32) -> Self { Self { lo, hi } }
}

#[cfg(not(feature = "spans"))]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span;
#[cfg(not(feature = "spans"))]
impl Span {
    pub const fn new(_: u32, _: u32) -> Self { Self }
}