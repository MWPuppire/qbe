#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused)]
#![allow(clippy::upper_case_acronyms)]
#![allow(clippy::useless_transmute)]

include!(concat!(env!("OUT_DIR"), "/qbe-bindings.rs"));

use crate::QbeTarget;
use errno::{errno, Errno};
use libc::*;
use std::ffi::CString;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr::NonNull;
use std::sync::Mutex;

#[export_name = "T"]
static mut TARGET: MaybeUninit<Target> = MaybeUninit::uninit();
#[export_name = "debug"]
static mut DEBUG: [c_char; 91] = [0; 91];

static mut OUTF: Mutex<*mut FILE> = Mutex::new(std::ptr::null_mut());

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum CFileMode {
    Read,
    Write,
    AppendOnly,
    ReadWrite,
    RwNew,
    ReadAppend,
}
impl CFileMode {
    const fn to_str(self) -> *const c_char {
        match self {
            Self::Read => "r\0".as_ptr() as *const c_char,
            Self::Write => "w\0".as_ptr() as *const c_char,
            Self::AppendOnly => "a\0".as_ptr() as *const c_char,
            Self::ReadWrite => "r+\0".as_ptr() as *const c_char,
            Self::RwNew => "w+\0".as_ptr() as *const c_char,
            Self::ReadAppend => "a+\0".as_ptr() as *const c_char,
        }
    }
}

#[derive(Eq, PartialEq)]
pub(crate) struct CFile(pub(crate) NonNull<FILE>);
impl CFile {
    pub fn open(name: &str, mode: CFileMode) -> Result<Self, Errno> {
        unsafe {
            let name = CString::new(name).expect("File names shouldn't contain a NULL");
            let file = fopen(name.as_ptr(), mode.to_str());
            if let Some(nonnull) = NonNull::new(file) {
                Ok(CFile(nonnull))
            } else {
                Err(errno())
            }
        }
    }

    pub fn temporary() -> Result<Self, Errno> {
        unsafe {
            let file = tmpfile();
            if let Some(nonnull) = NonNull::new(file) {
                Ok(CFile(nonnull))
            } else {
                Err(errno())
            }
        }
    }

    #[cfg(unix)]
    pub fn read_from_buffer(buf: &[u8]) -> Result<Self, Errno> {
        unsafe {
            let file = fmemopen(
                buf.as_ptr() as *mut c_void,
                buf.len() as size_t,
                CFileMode::Read.to_str(),
            );
            if let Some(nonnull) = NonNull::new(file) {
                Ok(CFile(nonnull))
            } else {
                Err(errno())
            }
        }
    }
    #[cfg(not(unix))]
    pub fn read_from_buffer(buf: &[u8]) -> Result<Self, Errno> {
        unsafe {
            let temp = Self::temporary()?;
            let written = fwrite(
                buf.as_ptr() as *const c_void,
                1,
                buf.len() as u64,
                temp.file(),
            );
            if written < buf.len() {
                return Err(errno());
            }
            if fseek(temp.file(), 0, SEEK_SET as c_int) != 0 {
                return Err(errno());
            }
            temp
        }
    }

    #[inline]
    pub fn file(&self) -> *mut FILE {
        self.0.as_ptr()
    }
    pub fn len(&self) -> Result<usize, Errno> {
        unsafe {
            let file = self.file();
            if fseek(file, 0, SEEK_END as c_int) != 0 {
                return Err(errno());
            }
            let len = ftell(file);
            if len < 0 {
                return Err(errno());
            }
            if fseek(file, 0, SEEK_SET as c_int) != 0 {
                return Err(errno());
            }
            Ok(len as usize)
        }
    }
}
impl Drop for CFile {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            // SAFETY: constructors for `CFile` fail rather than return an
            // invalid file.
            // just ignore it if it fails; `drop` can't return anything anyway
            fclose(self.0.as_ptr());
        }
    }
}

extern "C" fn data(d: *mut Dat) {
    unsafe {
        // SAFETY: `data` will only be called from `write_assembly_to_file` or
        // `write_assembly_to_string`, which acquires the lock (and blocks until
        // it can).
        let file = OUTF.get_mut().unwrap();
        emitdat(d, *file);
        if ((*d).type_ == Dat_DEnd) {
            // no good way to handle errors from within `data`, since QBE calls
            // it and doesn't have any return type
            fputs("\n\0".as_ptr() as *const c_char, *file);
            freeall();
        }
    }
}
extern "C" fn func(f: *mut Fn) {
    unsafe {
        // SAFETY: `func` will only be called from `write_assembly_to_file` or
        // `write_assembly_to_string`, which acquires the lock (and blocks until
        // it can).
        let file = OUTF.get_mut().unwrap();
        (T.abi0.unwrap())(f);
        fillrpo(f);
        fillpreds(f);
        filluse(f);
        promote(f);
        filluse(f);
        ssa(f);
        filluse(f);
        ssacheck(f);
        fillalias(f);
        loadopt(f);
        filluse(f);
        fillalias(f);
        coalesce(f);
        filluse(f);
        ssacheck(f);
        copy(f);
        filluse(f);
        fold(f);
        (T.abi1.unwrap())(f);
        simpl(f);
        fillpreds(f);
        filluse(f);
        (T.isel.unwrap())(f);
        fillrpo(f);
        filllive(f);
        filllive(f);
        filllive(f);
        fillloop(f);
        fillcost(f);
        spill(f);
        rega(f);
        fillrpo(f);
        simpljmp(f);
        fillpreds(f);
        fillrpo(f);
        for n in 0..(*f).nblk {
            if n == (*f).nblk - 1 {
                (*(*(*f).rpo.offset(n as isize))).link = std::ptr::null_mut();
            } else {
                (*(*(*f).rpo.offset(n as isize))).link = (*(*f).rpo.offset(n as isize + 1));
            }
        }
        (T.emitfn.unwrap())(f, *file);
        // no error handling, as mentioned in `data`
        fputs("\n\0".as_ptr() as *const c_char, *file);
        freeall();
    }
}

pub(crate) fn write_assembly_to_file(
    code: &str,
    target: QbeTarget,
    dest: &CFile,
) -> Result<(), Errno> {
    unsafe {
        let mut file = OUTF.lock().unwrap();
        *file = dest.file();

        DEBUG.fill(0);
        TARGET = MaybeUninit::new(target.into());

        let infile = CFile::read_from_buffer(code.as_bytes())?;
        parse(
            infile.file(),
            "-\0".as_ptr() as *mut c_char,
            Some(data),
            Some(func),
        );
        (T.emitfin.unwrap())(*file);
        Ok(())
    }
}

pub(crate) fn write_assembly_to_string(code: &str, target: QbeTarget) -> Result<String, Errno> {
    unsafe {
        let mut file = OUTF.lock().unwrap();
        let temp = CFile::temporary()?;
        *file = temp.file();

        DEBUG.fill(0);
        TARGET = MaybeUninit::new(target.into());

        let infile = CFile::read_from_buffer(code.as_bytes())?;
        parse(
            infile.file(),
            "-\0".as_ptr() as *mut c_char,
            Some(data),
            Some(func),
        );
        (T.emitfin.unwrap())(*file);

        let out_len = temp.len()?;
        let mut s = ManuallyDrop::new(String::with_capacity(out_len));
        let s_ptr = s.as_mut_ptr();
        let read = fread(s_ptr as *mut c_void, 1, out_len as u64, *file);
        if read < out_len as u64 {
            // drop the `String` to avoid leaking memory
            let _ = ManuallyDrop::into_inner(s);
            return Err(errno());
        }
        // I'm conflicted about how to handle this. On the one hand, this is
        // definitely an error, and returning `Ok` would leave no way to detect
        // or handle the error; but on the other hand, the string is correct by
        // this point, failure to release file handles isn't typically a
        // significant problem, and `fclose` isn't always checked for errors
        // anyway in my code (see, e.g., `CFile::drop`).
        if fclose(*file) != 0 {
            // drop the `String` to avoid leaking memory
            let _ = ManuallyDrop::into_inner(s);
            return Err(errno());
        }
        let cap = s.capacity();
        Ok(String::from_raw_parts(s_ptr, out_len, cap))
    }
}

extern "C" {
    static T_amd64_sysv: Target;
    static T_amd64_apple: Target;
    static T_arm64: Target;
    static T_arm64_apple: Target;
    static T_rv64: Target;
}

impl From<QbeTarget> for Target {
    #[inline]
    fn from(item: QbeTarget) -> Target {
        unsafe {
            match item {
                QbeTarget::Amd64 => T_amd64_sysv,
                QbeTarget::Amd64Apple => T_amd64_apple,
                QbeTarget::Arm64 => T_arm64,
                QbeTarget::Arm64Apple => T_arm64_apple,
                QbeTarget::RiscV64 => T_rv64,
            }
        }
    }
}
