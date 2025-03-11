//! SID - String ID Library
//! 
//! A high-performance string ID system for Rust applications,
//! particularly suited for game development.

use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
    num::NonZeroU64,
};

#[cfg(feature = "bevy")]
use bevy::reflect::{FromReflect, Reflect};
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub use macros::sid_iter;

/// Creates a SID from a string literal at compile time
/// 
/// # Examples
/// 
/// ```
/// use sid::sid;
/// 
/// const PLAYER_ID: SID = sid!("Player");
/// ```
#[macro_export]
macro_rules! sid {
    ($s:literal) => {
        $crate::const_sid_from_str($s)
    };
}



// SAFETY / SANITY - NEVER MAKE THIS A USIZE BECAUSE THEN PLATFORMS WOULD HAVE DIFFERENT VALUES FOR
// DIFFERENT SIDS - BAD!
pub type SIDHashInteger = u64;
pub type NonZeroSIDHashInteger = NonZeroU64;

/// Static Map to allow for occasional, ergonimic reads of SIDs, e.g. error messages / debugging
#[derive(Debug, Default)]
pub struct SIDMap(flurry::HashMap<SID, &'static str, ahash::RandomState>);

static SID_LOOKUP: OnceCell<&'static SIDMap> = OnceCell::new();

/// Lookup for DashMap of Strings for string ids [[SID]]
fn sid_lookup() -> &'static SIDMap {
    // should this be a Either<Box<str>, &'static str> instead of a Cow
    SID_LOOKUP.get().expect("SID_LOOKUP was not initialized")
}

fn sid_check(reference: &'static SIDMap) {
    let _x: SIDHashInteger = 5;
    NonZeroSIDHashInteger::try_from(5).unwrap();
    assert!(
        std::mem::size_of::<SIDHashInteger>()
            == std::mem::size_of::<Option<NonZeroSIDHashInteger>>()
    );
    assert!(std::mem::size_of::<SIDHashInteger>() <= 16); // at most 16 bytes since that is output
    assert!(std::mem::size_of::<SIDHashInteger>() >= 8); // at least 8 bytes since we need that
                                                         // much for fn first_64_bits()
                                                         // SID_LOOKUP.set(reference).expect("
                                                         // SID_LOOKUP can only be set once")
}

pub fn sid_try_init(reference: &'static SIDMap) -> Result<(), SIDError> {
    let _x: SIDHashInteger = 5;
    NonZeroSIDHashInteger::try_from(5).unwrap();
    assert!(
        std::mem::size_of::<SIDHashInteger>()
            == std::mem::size_of::<Option<NonZeroSIDHashInteger>>()
    );
    assert!(std::mem::size_of::<SIDHashInteger>() <= 16); // at most 16 bytes since that is output
    assert!(std::mem::size_of::<SIDHashInteger>() >= 8); // at least 8 bytes since we need that
                                                         // much for fn first_64_bits()
    match SID_LOOKUP.set(reference) {
        Ok(()) => Ok(()),
        Err(_) => Err(SIDError::InitMultipleTimes),
    }
    //.expect("SID_LOOKUP can only be set once")
}

pub fn sid_get_or_init() -> &'static SIDMap {
    SID_LOOKUP.get_or_init(|| {
        let boxed = Box::<SIDMap>::default();
        let static_ref: &'static _ = Box::leak(boxed);
        sid_check(static_ref);
        static_ref
    })
}

#[derive(Debug, Error)]
pub enum SIDError {
    #[error("SIDMap has already been set")]
    InitMultipleTimes,
}

#[allow(unused_must_use)]
#[doc(hidden)]
#[cfg(debug_assertions)]
pub fn sid_init_for_tests() {
    let _x: SIDHashInteger = 5;
    NonZeroSIDHashInteger::try_from(5).unwrap();
    assert!(std::mem::size_of::<SIDHashInteger>() <= 16);
    assert!(std::mem::size_of::<SIDHashInteger>() >= 8);
    assert!(
        std::mem::size_of::<SIDHashInteger>()
            == std::mem::size_of::<Option<NonZeroSIDHashInteger>>()
    );
    static TEST_SID_MAP: OnceCell<SIDMap> = OnceCell::new();
    SID_LOOKUP.set(TEST_SID_MAP.get_or_init(SIDMap::default));
}

/// A string hash or String ID
/// Used for fast (single integer) equality comparisons with guaranteed uniqueness, checked globally against other SIDs
/// at hash-time
#[cfg_attr(
    feature = "bevy",
    derive(Reflect, FromReflect),
    reflect_value(PartialEq, Serialize, Deserialize, Hash)
)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize, PartialOrd, Ord)]
#[serde(from = "String")]
#[serde(into = "String")]
pub struct SID {
    hash: SIDHashInteger,
}

impl From<SID> for String {
    fn from(s: SID) -> Self { s.get_string().unwrap_or_else(|| format!("{:?}", s.hash)) }
}

impl From<SID> for &'static str {
    fn from(s: SID) -> Self { s.get_str().unwrap_or("Unknown SID") }
}

impl<T: AsRef<str>> From<T> for SID {
    fn from(s: T) -> Self {
        match SID::internal_register(&s) {
            SIDResult::Ok(sid) => sid,
            SIDResult::AlreadyRegistered(sid) => sid,
        }
    }
}

enum SIDResult {
    Ok(SID),
    AlreadyRegistered(SID),
}
impl SID {
    #[doc(hidden)]
    pub const fn first_64_bits(&self) -> u64 {
        u64::from_le_bytes(first_n_bytes(&self.hash.to_le_bytes()))
    }
    fn internal_register(s: impl AsRef<str>) -> SIDResult {
        let sid = const_sid_from_str(s.as_ref());
        if let Some(v) = sid_lookup().0.pin().get(&sid) {
            if v != &s.as_ref() {
                panic!("SID Collision: {sid:?}; {v} != {}", s.as_ref());
            } else {
                return SIDResult::AlreadyRegistered(sid);
            }
        } else {
            let boxed: Box<str> = s.as_ref().into();
            let static_str: &'static str = Box::leak(boxed);
            sid_lookup().0.pin().insert(sid, static_str);
        }
        SIDResult::Ok(sid)
    }

    /// Lookup for Strings for string ids [[SID]]
    fn get_string(&self) -> Option<String> { sid_lookup().0.pin().get(self).map(|s| s.to_string()) }

    /// Returns the &str representation of the SID if it exists.
    fn get_str(&self) -> Option<&'static str> { sid_lookup().0.pin().get(self).copied() }

    /// Returns a &'static str of the SID
    /// # Panics if the SID hasn't been registered
    pub fn to_str(&self) -> &'static str { self.get_str().unwrap() }

    #[allow(unused_must_use)]
    /// Convert a slice of string-types to SIDs
    pub fn register_slice<T>(input: &[T])
    where T: AsRef<str> + Debug + ToString + Display {
        for string in input {
            SID::register(string);
        }
    }

    /// Register a string in sid lookup
    pub fn register(s: impl AsRef<str>) {
        if let SIDResult::AlreadyRegistered(sid) = SID::internal_register(&s) {
            #[cfg(debug_assertions)]
            eprintln!("SID ({sid:?}) has already been registered: {}", s.as_ref());
        }
    }
}

const fn first_n_bytes<const N: usize>(input: &[u8]) -> [u8; N] {
    assert!(input.len() >= N);
    let mut output: [u8; N] = [0; N];
    let mut i = 0;
    loop {
        if i == N {
            break output;
        }
        output[i] = input[i];
        i += 1;
    }
}

#[test]
fn test_first_n_bytes() {
    let x = 1u32;
    #[cfg(target_endian = "little")]
    let byte = first_n_bytes::<1>(&x.to_le_bytes());
    #[cfg(target_endian = "big")]
    let byte = first_n_bytes::<1>(&x.to_be_bytes());
    assert_eq!(byte[0], 1u8)
}

/// Hashes a string into a SID at compile time
/// Use carefully since are not registered with the [SIDMap]
pub const fn const_sid_from_str(string: &str) -> SID {
    let bytes = lhash::md5(string.as_bytes());
    const N: usize = std::mem::size_of::<SIDHashInteger>();
    SID { hash: SIDHashInteger::from_le_bytes(first_n_bytes::<N>(&bytes)) }
}

/// Optionally hashes a string at compile time
/// Use carefully since are not registered with the [SIDMap]
pub const fn option_const_sid_from_str(string: Option<&'static str>) -> Option<SID> {
    if let Some(string) = string {
        Some(const_sid_from_str(string))
    } else {
        None
    }
}

impl Display for SID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(string) =
            SID_LOOKUP.get().expect("SID_LOOKUP was not initialized").0.pin().get(self)
        {
            // write!(f, "{}", <Box<str> as AsRef<str>>::as_ref(string))
            write!(f, "{}", string)
        } else {
            write!(f, "Unkown SID: {self:?}")
        }
    }
}

impl Debug for SID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(string) =
            SID_LOOKUP.get().expect("SID_LOOKUP was not initialized").0.pin().get(self)
        {
            write!(f, "DebugSID: {}", string)
            //<Box<str> as AsRef<str>>::as_ref(string))
        } else {
            write!(f, "Unkown SID: {:?}", self.hash)
        }
    }
}
