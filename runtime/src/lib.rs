use std::alloc::{alloc, dealloc, Layout};
use std::cell::RefCell;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::ptr;

#[repr(C)]
struct GcHeader {
    marked: u8,
    _pad: [u8; 7],
    size: usize,
    next: *mut GcHeader,
}

#[repr(C)]
struct IntArrayHeader {
    len: i32,
}

struct OwnedRoot {
    slot: *mut *mut u8,
}

struct GcState {
    head: *mut GcHeader,
    root_slots: Vec<*mut *mut u8>,
    owned_roots: Vec<OwnedRoot>,
}

impl GcState {
    const fn new() -> Self {
        Self {
            head: ptr::null_mut(),
            root_slots: Vec::new(),
            owned_roots: Vec::new(),
        }
    }

    unsafe fn alloc(&mut self, size: usize) -> *mut u8 {
        let total = size
            .checked_add(std::mem::size_of::<GcHeader>())
            .expect("GC alloc overflow");

        let layout = Layout::from_size_align(total, std::mem::align_of::<usize>()).unwrap();
        let raw = alloc(layout) as *mut GcHeader;
        if raw.is_null() {
            eprintln!("picolang_runtime: out of memory");
            std::process::abort();
        }

        (*raw).marked = 0;
        (*raw).size = size;
        (*raw).next = self.head;
        self.head = raw;

        raw.add(1) as *mut u8
    }

    unsafe fn add_root_slot(&mut self, slot: *mut *mut u8) {
        if !slot.is_null() {
            self.root_slots.push(slot);
        }
    }

    unsafe fn remove_root_slot(&mut self, slot: *mut *mut u8) {
        if slot.is_null() {
            return;
        }
        if let Some(idx) = self.root_slots.iter().position(|&p| p == slot) {
            self.root_slots.swap_remove(idx);
        }
    }

    unsafe fn add_root_value(&mut self, ptr_value: *mut u8) {
        let b = Box::new(ptr_value);
        let slot = Box::into_raw(b) as *mut *mut u8;
        self.root_slots.push(slot);
        self.owned_roots.push(OwnedRoot { slot });
    }

    unsafe fn remove_root_value(&mut self, ptr_value: *mut u8) {
        if let Some(idx) = self.owned_roots.iter().position(|r| {
            let slot = r.slot;
            !slot.is_null() && unsafe { *slot } == ptr_value
        }) {
            let slot = self.owned_roots.swap_remove(idx).slot;

            if let Some(j) = self.root_slots.iter().position(|&p| p == slot) {
                self.root_slots.swap_remove(j);
            }

            drop(unsafe { Box::from_raw(slot as *mut *mut u8) });
        }
    }

    unsafe fn collect(&mut self) {
        let slots: Vec<*mut *mut u8> = self.root_slots.clone();

        // Mark
        for slot in slots {
            if slot.is_null() {
                continue;
            }
            let root = *slot;
            self.mark_from(root);
        }

        // Sweep
        let mut cur = self.head;
        let mut prev: *mut GcHeader = ptr::null_mut();

        while !cur.is_null() {
            let next = (*cur).next;
            if (*cur).marked == 0 {
                let total = (*cur)
                    .size
                    .checked_add(std::mem::size_of::<GcHeader>())
                    .unwrap();
                let layout = Layout::from_size_align(total, std::mem::align_of::<usize>()).unwrap();

                if !prev.is_null() {
                    (*prev).next = next;
                } else {
                    self.head = next;
                }

                dealloc(cur as *mut u8, layout);
            } else {
                (*cur).marked = 0;
                prev = cur;
            }
            cur = next;
        }
    }

    unsafe fn mark_from(&mut self, payload: *mut u8) {
        if payload.is_null() {
            return;
        }

        let header = (payload as *mut GcHeader).sub(1);
        if (*header).marked != 0 {
            return;
        }
        (*header).marked = 1;

        let word_size = std::mem::size_of::<usize>();
        let words = (*header).size / word_size;
        let mut cur = payload as *mut usize;
        let end = cur.add(words);

        while cur < end {
            let candidate = *cur as *mut u8;
            if !candidate.is_null() {
                let mut obj = self.head;
                while !obj.is_null() {
                    let obj_start = obj.add(1) as *mut u8;
                    let obj_end = obj_start.add((*obj).size);
                    if candidate >= obj_start && candidate < obj_end {
                        self.mark_from(obj_start);
                        break;
                    }
                    obj = (*obj).next;
                }
            }
            cur = cur.add(1);
        }
    }
}

thread_local! {
    static GC: RefCell<GcState> = RefCell::new(GcState::new());
}

#[no_mangle]
pub extern "C" fn picolang_gc_alloc(size: usize) -> *mut u8 {
    GC.with(|gc| unsafe { gc.borrow_mut().alloc(size) })
}

#[no_mangle]
pub extern "C" fn picolang_gc_collect() {
    GC.with(|gc| unsafe { gc.borrow_mut().collect() })
}

#[no_mangle]
pub extern "C" fn picolang_gc_add_root_slot(slot: *mut *mut u8) {
    GC.with(|gc| unsafe { gc.borrow_mut().add_root_slot(slot) })
}

#[no_mangle]
pub extern "C" fn picolang_gc_remove_root_slot(slot: *mut *mut u8) {
    GC.with(|gc| unsafe { gc.borrow_mut().remove_root_slot(slot) })
}

#[no_mangle]
pub extern "C" fn picolang_gc_add_root(ptr_value: *mut u8) {
    GC.with(|gc| unsafe { gc.borrow_mut().add_root_value(ptr_value) })
}

#[no_mangle]
pub extern "C" fn picolang_gc_remove_root(ptr_value: *mut u8) {
    GC.with(|gc| unsafe { gc.borrow_mut().remove_root_value(ptr_value) })
}

#[no_mangle]
pub extern "C" fn runtime_obj_alloc(size: i32) -> *mut u8 {
    let mut sz = if size < 0 { 0usize } else { size as usize };
    if sz < 16 {
        sz = 16;
    }

    let p = picolang_gc_alloc(sz);
    if !p.is_null() {
        unsafe { ptr::write_bytes(p, 0u8, sz) };
    }
    p
}

#[no_mangle]
pub extern "C" fn runtime_store_i32(obj: *mut u8, off: i32, value: i32) {
    if obj.is_null() || off < 0 {
        return;
    }

    unsafe {
        let header = (obj as *mut GcHeader).sub(1);
        let size = (*header).size;
        let o = off as usize;

        if o.checked_add(std::mem::size_of::<i32>()).map_or(true, |end| end > size) {
            return;
        }

        let p = obj.add(o) as *mut i32;
        ptr::write_unaligned(p, value);
    }
}

#[no_mangle]
pub extern "C" fn runtime_load_i32(obj: *mut u8, off: i32) -> i32 {
    if obj.is_null() || off < 0 {
        return 0;
    }

    unsafe {
        let header = (obj as *mut GcHeader).sub(1);
        let size = (*header).size;
        let o = off as usize;

        if o.checked_add(std::mem::size_of::<i32>()).map_or(true, |end| end > size) {
            return 0;
        }

        let p = obj.add(o) as *const i32;
        ptr::read_unaligned(p)
    }
}

#[no_mangle]
pub extern "C" fn runtime_array_new_int(len: i32) -> *mut u8 {
    let len = if len < 0 { 0 } else { len as usize };
    let header_size = std::mem::size_of::<IntArrayHeader>();
    let elem_size = std::mem::size_of::<i32>();
    let payload_size = header_size + len * elem_size;

    GC.with(|gc| unsafe {
        let payload = gc.borrow_mut().alloc(payload_size);

        let header = payload as *mut IntArrayHeader;
        (*header).len = len as i32;

        let data_ptr = (payload as *mut u8).add(header_size) as *mut i32;
        for i in 0..len {
            *data_ptr.add(i) = 0;
        }

        payload
    })
}

#[no_mangle]
pub extern "C" fn runtime_array_len_int(arr: *mut u8) -> i32 {
    if arr.is_null() {
        return 0;
    }
    unsafe { (*(arr as *mut IntArrayHeader)).len }
}

#[no_mangle]
pub extern "C" fn runtime_array_get_int(arr: *mut u8, idx: i32) -> i32 {
    if arr.is_null() {
        return 0;
    }
    unsafe {
        let header = arr as *mut IntArrayHeader;
        let len = (*header).len;
        if idx < 0 || idx >= len {
            return 0;
        }
        let header_size = std::mem::size_of::<IntArrayHeader>();
        let data_ptr = (arr as *mut u8).add(header_size) as *mut i32;
        *data_ptr.add(idx as usize)
    }
}

#[no_mangle]
pub extern "C" fn runtime_array_set_int(arr: *mut u8, idx: i32, value: i32) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let header = arr as *mut IntArrayHeader;
        let len = (*header).len;
        if idx < 0 || idx >= len {
            return;
        }
        let header_size = std::mem::size_of::<IntArrayHeader>();
        let data_ptr = (arr as *mut u8).add(header_size) as *mut i32;
        *data_ptr.add(idx as usize) = value;
    }
}

#[no_mangle]
pub extern "C" fn runtime_string_concat(a: *const u8, b: *const u8) -> *mut u8 {
    let a = if a.is_null() { b"" } else { unsafe { CStr::from_ptr(a as *const c_char).to_bytes() } };
    let b = if b.is_null() { b"" } else { unsafe { CStr::from_ptr(b as *const c_char).to_bytes() } };

    let total = a.len().saturating_add(b.len()).saturating_add(1);
    let out = runtime_obj_alloc(total as i32);
    if out.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        ptr::copy_nonoverlapping(a.as_ptr(), out, a.len());
        ptr::copy_nonoverlapping(b.as_ptr(), out.add(a.len()), b.len());
        *out.add(a.len() + b.len()) = 0;
    }

    out
}

#[no_mangle]
pub extern "C" fn abs(x: i32) -> i32 {
    x.wrapping_abs()
}

#[no_mangle]
pub extern "C" fn printInt(x: i32) {
    println!("{x}");
}

#[no_mangle]
pub extern "C" fn printChar(ch: u8) {
    use std::io::{self, Write};
    let c = ch as char;
    print!("{c}");
    let _ = io::stdout().flush();
}

#[no_mangle]
pub extern "C" fn printString(ptr: *const u8) {
    use std::io::{self, Write};

    if ptr.is_null() {
        return;
    }

    unsafe {
        let c_str = CStr::from_ptr(ptr as *const c_char);
        if let Ok(s) = c_str.to_str() {
            print!("{s}");
            let _ = io::stdout().flush();
        }
        println!();
    }
}

#[no_mangle]
pub extern "C" fn print(ptr: *const u8) {
    printString(ptr);
}

#[no_mangle]
pub extern "C" fn log_logInfo(ptr: *const u8) {
    if ptr.is_null() {
        return;
    }

    unsafe {
        let c_str = CStr::from_ptr(ptr as *const c_char);
        if let Ok(s) = c_str.to_str() {
            eprintln!("[info] {s}");
        }
    }
}

#[no_mangle]
pub extern "C" fn runtime_read_file(path: *const u8) -> *mut u8 {
    if path.is_null() {
        return ptr::null_mut();
    }

    let filename = unsafe {
        let c_str = CStr::from_ptr(path as *const c_char);
        match c_str.to_str() {
            Ok(s) => s,
            Err(_) => return ptr::null_mut(),
        }
    };

    let data = match std::fs::read(filename) {
        Ok(d) => d,
        Err(_) => return ptr::null_mut(),
    };

    let len = data.len();
    let out = runtime_obj_alloc((len + 1) as i32);
    if out.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        ptr::copy_nonoverlapping(data.as_ptr(), out, len);
        *out.add(len) = 0;
    }

    out
}

#[no_mangle]
pub extern "C" fn openFile(path: *const u8) -> *mut u8 {
    runtime_read_file(path)
}
