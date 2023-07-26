#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused)]

include!(concat!(env!("OUT_DIR"), "/qbe-bindings.rs"));

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

pub(crate) struct CFile(pub(crate) NonNull<FILE>);
impl CFile {
    pub(crate) fn open(name: &str, mode: &str) -> Result<Self, Errno> {
        unsafe {
            let name = CString::new(name).expect("File names shouldn't contain a NULL");
            let file = fopen(
                name.as_ptr(),
                mode.as_ptr() as *const c_char,
            );
            if let Some(nonnull) = NonNull::new(file) {
                Ok(CFile(nonnull))
            } else {
                Err(errno())
            }
        }
    }
    pub(crate) fn temporary() -> Result<Self, Errno> {
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
    pub(crate) fn read_from_buffer(buf: &[u8]) -> Result<Self, Errno> {
        unsafe {
            let file = fmemopen(
                buf.as_ptr() as *mut c_void,
                buf.len() as size_t,
                "r\0".as_ptr() as *const c_char,
            );
            if let Some(nonnull) = NonNull::new(file) {
                Ok(CFile(nonnull))
            } else {
                Err(errno())
            }
        }
    }
    #[cfg(not(unix))]
    pub(crate) fn read_from_buffer(buf: &[u8]) -> Result<Self, Errno> {
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
    pub(crate) fn file(&self) -> *mut FILE {
        self.0.as_ptr()
    }
    pub(crate) fn len(&self) -> Result<usize, Errno> {
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
        TARGET = MaybeUninit::new(target.target());

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
        TARGET = MaybeUninit::new(target.target());

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
            return Err(errno());
        }
        let cap = s.capacity();
        fclose(*file);
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

pub enum QbeTarget {
    Amd64,
    Amd64Apple,
    Arm64,
    Arm64Apple,
    RiscV64,
}
impl QbeTarget {
    #[inline]
    fn target(&self) -> Target {
        unsafe {
            match self {
                Self::Amd64 => T_amd64_sysv,
                Self::Amd64Apple => T_amd64_apple,
                Self::Arm64 => T_arm64,
                Self::Arm64Apple => T_arm64_apple,
                Self::RiscV64 => T_rv64,
            }
        }
    }
}
impl Default for QbeTarget {
    #[inline]
    fn default() -> Self {
        cfg_if::cfg_if! {
            if #[cfg(all(target_arch = "x86_64", any(target_os = "macos", target_os = "ios")))] {
                Self::Amd64Apple
            } else if #[cfg(target_arch = "x86_64")] {
                Self::Amd64Sysv
            } else if #[cfg(all(target_arch = "aarch64", any(target_os = "macos", target_os = "ios")))] {
                Self::Arm64Apple
            } else if #[cfg(target_arch = "aarch64")] {
                Self::Arm64
            } else if #[cfg(target_arch = "riscv64gc")] {
                Self::RiscV64
            } else {
                Self::Amd64Sysv // Idk, seems like a reasonable default
            }
        }
    }
}
