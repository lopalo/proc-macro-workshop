// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
use bitfield_impl::define_specifiers;
pub use bitfield_impl::{bitfield, BitfieldSpecifier};
pub use const_format;

pub trait Specifier {
    const BITS: usize;
    type Item;
    type ItemBytes;
    const ZERO_ITEM_BYTES: Self::ItemBytes;

    fn item_to_bytes(item: Self::Item) -> Self::ItemBytes;
    fn item_from_bytes(bytes: Self::ItemBytes) -> Self::Item;
}

define_specifiers! {B 1 8 u8}
define_specifiers! {B 9 16 u16}
define_specifiers! {B 17 32 u32}
define_specifiers! {B 33 64 u64}

impl Specifier for bool {
    const BITS: usize = 1;
    type Item = Self;
    type ItemBytes = [u8; 1];
    const ZERO_ITEM_BYTES: [u8; 1] = [0; 1];

    fn item_to_bytes(item: bool) -> [u8; 1] {
        (item as u8).to_le_bytes()
    }

    fn item_from_bytes(bytes: [u8; 1]) -> bool {
        u8::from_le_bytes(bytes) != 0
    }
}

/// Panics if the bits are out of bounds of either `data` or `item`
pub fn write_bits(
    data: &mut [u8],
    data_offset_bits: usize,
    item: &mut [u8],
    item_bits: usize,
) {
    for item_byte in &mut *item {
        *item_byte = item_byte.reverse_bits();
    }

    let mask: u8 = 0b10000000;
    for item_bit_idx in 0..item_bits {
        let data_bit_idx = data_offset_bits + item_bit_idx;

        let data_byte_idx = data_bit_idx / 8;
        let data_byte_bit_idx = data_bit_idx % 8;

        let item_byte_idx = item_bit_idx / 8;
        let item_byte_bit_idx = item_bit_idx % 8;

        let data_byte = data[data_byte_idx];
        let item_byte = item[item_byte_idx];
        let bit_is_set = (item_byte << item_byte_bit_idx) & mask == mask;

        data[data_byte_idx] = if bit_is_set {
            data_byte | (mask >> data_byte_bit_idx)
        } else {
            data_byte & !(mask >> data_byte_bit_idx)
        };
    }
}

/// `item` must be zeroed.
/// Panics if the bits are out of bounds of either `data` or `item`
pub fn read_bits(
    data: &[u8],
    data_offset_bits: usize,
    item: &mut [u8],
    item_bits: usize,
) {
    let mask: u8 = 0b10000000;
    for item_bit_idx in 0..item_bits {
        let data_bit_idx = data_offset_bits + item_bit_idx;

        let data_byte_idx = data_bit_idx / 8;
        let data_byte_bit_idx = data_bit_idx % 8;
        let data_byte = data[data_byte_idx];

        let bit_is_set = (data_byte << data_byte_bit_idx) & mask == mask;
        if !bit_is_set {
            continue;
        }

        let item_byte_idx = item_bit_idx / 8;
        let item_byte_bit_idx = item_bit_idx % 8;
        let item_byte = item[item_byte_idx];
        item[item_byte_idx] = item_byte | (mask >> item_byte_bit_idx);
    }

    for item_byte in item {
        *item_byte = item_byte.reverse_bits();
    }
}
